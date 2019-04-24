#lang racket/base

(module+ for-public
  (provide
   &bg &pipeline-ret &env
   &in &< &out &> &>! &>> &err
   &strict &permissive &lazy &lazy-timeout
   pipeline-start-segment
   pipeline-joint-segment
   run-pipeline
   with-pipeline-config
   splicing-with-pipeline-config
   (for-syntax
    pipeline-starter
    pipeline-joint
    )
   ))

(provide
 ;pipeline-start-segment
 ;pipeline-joint-segment
 run-pipeline
 with-pipeline-config
 splicing-with-pipeline-config
 &bg &pipeline-ret &env &env-replace &in &< &out &> &>! &>> &err
 )

(require
 syntax/parse
 racket/stxparam
 racket/splicing
 racket/string
 racket/port
 (prefix-in mp: shell/mixed-pipeline)
 "pipeline-operator-default.rkt"
 "pipeline-operators.rkt"
 "../utils/bourne-expansion-utils.rkt"
 "pipeline-operator-generics.rkt"
 (for-syntax
  racket/base
  syntax/parse
  racket/stxparam-exptime
  syntax/keyword
  racket/dict
  "misc-utils.rkt"
  (for-syntax
   racket/base
   syntax/parse
   )))

(define-syntax (def-pipeline-opt stx)
  (syntax-parse stx
    [(_ name)
     #'(define-syntax-parameter name
         (syntax-id-rules
             ()
           [e (raise-syntax-error
               'name
               "can't be used except at the beginning of a rash pipeline specification"
               #'e)]))]))
(def-pipeline-opt &bg)
(def-pipeline-opt &pipeline-ret) ;; TODO - name?
(def-pipeline-opt &env)
(def-pipeline-opt &env-replace)
(def-pipeline-opt &in)
(def-pipeline-opt &<)
(def-pipeline-opt &out)
(def-pipeline-opt &>)
(def-pipeline-opt &>!)
(def-pipeline-opt &>>)
(def-pipeline-opt &err)
(def-pipeline-opt &strict)
(def-pipeline-opt &permissive)
(def-pipeline-opt &lazy)
(def-pipeline-opt &lazy-timeout)


(begin-for-syntax
  (define-literal-set pipeline-opts
    (&bg &pipeline-ret &env &env-replace
         &in &< &out &> &>! &>> &err
         &strict &permissive &lazy &lazy-timeout))
  (define-syntax-class not-opt
    #:literal-sets (pipeline-opts)
    (pattern (~not (~or &bg &pipeline-ret &env &env-replace
                        &in &out &err &< &> &>! &>>
                        &strict &permissive &lazy &lazy-timeout)))))

;; TODO - maybe these should be used by mixed-pipeline as well?
(define-syntax-parameter default-pipeline-in
  (syntax-parser [_ #'(open-input-string "")]))
(define-syntax-parameter default-pipeline-out
  (syntax-parser [_ #'(λ (x) (string-trim (begin0 (port->string x)
                                            (close-input-port x))))]))
(define-syntax-parameter default-pipeline-err-out
  (syntax-parser [_ #'mp:stderr-capture-redirect]))

(define-for-syntax (with-pipeline-config* stx let-form parameterization-form)
  ;; TODO - the in out and err expressions need to be lexical and not syntax-parameter-y too.
  ;; TODO - all the pipeline ops need parameterizable lexical defaults.
  ;; TODO - the name "parameters" is probably bad, since they should not be syntax-parameters...
  ;; TODO - the code to get the context is copy/pasted...  I need to generalize it, because it's used in with-pipeline-config, with-rash-config, and with-default-line-macro.
  (syntax-parse stx
    [(orig-macro
      (~or (~optional (~seq #:in in))
           (~optional (~seq #:out out))
           (~optional (~seq #:err err))
           ;(~optional (~seq #:starter starter:pipeline-starter))
           (~optional (~seq #:starter starter))
           (~optional (~seq #:context context)))
      ...
      body:expr ...+)
     (let* ([set-in (and (attribute in)
                         #'(default-pipeline-in (λ (stx) (quote-syntax in))))]
            [set-out (and (attribute out)
                          #'(default-pipeline-out (λ (stx) (quote-syntax out))))]
            [set-err (and (attribute err)
                          #'(default-pipeline-err-out (λ (stx) (quote-syntax err))))]
            [starter-context-id
             (or (attribute context)
                 (let ([cs (map (λ (x) (datum->syntax x '#%app #'orig-macro))
                                (syntax->list #'(body ...)))])
                   (unless (or (not (attribute starter))
                               (for/and ([x (cdr cs)])
                                 (bound-identifier=? (car cs) x)))
                     (raise-syntax-error
                      'with-pipeline-config
                      "Multiple body forms were given with different scoping information, so there is not a clear choice of info to bind the default pipeline starter to."
                      stx))
                   (car cs)))]
            [set-starter (and (attribute starter)
                              #`(#,(datum->syntax
                                    starter-context-id
                                    '#%shell-pipeline/default-pipeline-starter
                                    (attribute starter))
                                 (make-rename-transformer
                                  (quote-syntax starter))))]
            [parameterizations
             #`(#,@(filter (λ(x)x) (list set-in set-out set-err)))]
            [lets
             #`(#,@(filter (λ(x)x) (list set-starter)))])
       #`(#,let-form
          #,lets
          (#,parameterization-form
           #,parameterizations
           body ...)))]))
(begin-for-syntax
  (define-splicing-syntax-class kw-opt
    (pattern (~seq kw:keyword val:expr))))
(define-syntax (with-pipeline-config stx)
  (with-pipeline-config* stx #'let-syntax #'syntax-parameterize))
(define-syntax (splicing-with-pipeline-config stx)
  (with-pipeline-config* stx #'splicing-let-syntax #'splicing-syntax-parameterize))

(define-syntax (pipeline-start-segment stx)
  (syntax-parse stx
    [(_ arg ...+)
     #'(rash-pipeline-splitter/start first-class-split-pipe/start () arg ...)]))
(define-syntax (pipeline-joint-segment stx)
  (syntax-parse stx
    [(_ arg ...+)
     #'(rash-pipeline-splitter/joints first-class-split-pipe/joint () () (arg ...))]))

(define-syntax (run-pipeline stx)
  (syntax-parse stx
    [(_ arg ...)
     #'(run-pipeline/split arg ...)]))

(define-syntax (run-pipeline/split stx)
  ;; Parse out the beginning/ending whole-pipeline options, then
  ;; pass the pipeline specs to other macros to deal with.
  (syntax-parse stx
    [(_ rash-arg ...)
     (syntax-parse #'(rash-arg ...)
       #:literal-sets (pipeline-opts)
       [((~or (~optional (~and s-bg &bg) #:name "&bg option")
              (~optional (~and s-pr &pipeline-ret) #:name "&pipeline-ret option")
              (~optional (~seq &env s-env-list:expr) #:name "&env option")
              (~optional (~seq &env-replace s-env-r-list:expr)
                         #:name "&env-replace option")
              (~optional (~or (~and s-strict &strict)
                              (~and s-permissive &permissive)
                              (~and s-lazy &lazy))
                         #:name "&strict, &lazy, and &permissive options")
              (~optional (~seq &lazy-timeout s-lazy-timeout)
                         #:name "&lazy-timeout option")
              (~optional (~or (~seq &in s-in:expr)
                              (~seq &< s-<:expr))
                         #:name "&in and &< options")
              (~optional (~or (~seq &out s-out:expr)
                              (~seq &> s->:expr)
                              (~seq &>! s->!:expr)
                              (~seq &>> s->>:expr))
                         #:name "&out, &>, &>!, and &>> options")
              (~optional (~seq &err s-err:expr) #:name "&err option")
              )
         ...
         args1-head:not-opt args1 ...)
        ;; Now let's parse those in reverse at the end, so options are allowed at the beginning OR the end
        (syntax-parse (datum->syntax stx (reverse (syntax->list #'(args1-head args1 ...))))
          #:literal-sets (pipeline-opts)
          [((~or (~optional (~and e-bg &bg) #:name "&bg option")
                 (~optional (~and e-pr &pipeline-ret) #:name "&pipeline-ret option")
                 (~optional (~seq e-env-list:expr &env) #:name "&env option")
                 (~optional (~seq e-env-r-list:expr &env-replace)
                            #:name "&env-replace option")
                 (~optional (~or (~and e-strict &strict)
                                 (~and e-permissive &permissive)
                                 (~and e-lazy &lazy))
                            #:name "&strict, &lazy, and &permissive options")
                 (~optional (~seq e-lazy-timeout &lazy-timeout)
                            #:name "&lazy-timeout option")
                 (~optional (~or (~seq e-in:expr &in)
                                 (~seq e-<:expr &<))
                            #:name "&in and &< options")
                 (~optional (~or (~seq e-out:expr &out)
                                 (~seq e->:expr &>)
                                 (~seq e->!:expr &>!)
                                 (~seq e->>:expr &>>))
                            #:name "&out, &>, &>!, and &>> options")
                 (~optional (~seq e-err:expr &err) #:name "&err option")
                 )
            ...
            argrev-head:not-opt argrev ...)
           (syntax-parse (datum->syntax stx (reverse (syntax->list #'(argrev-head argrev ...))))
             [(arg ...)
              ;; Let's go more meta to clean up some error checking code...
              (define-syntax (noboth stx1)
                (syntax-parse stx1
                  [(_ (a ...) (b ...))
                   #'(when (and (or (attribute a) ...) (or (attribute b) ...))
                       (raise-syntax-error
                        'run-pipeline
                        "duplicated occurences of pipeline options at beginning and end"
                        stx))]
                  [(rec a:id b:id)
                   #'(rec (a) (b))]))
              (noboth s-bg e-bg)
              (noboth s-pr e-pr)
              (noboth s-env-list e-env-list)
              (noboth s-env-r-list e-env-r-list)
              (noboth s-lazy-timeout e-lazy-timeout)
              (noboth (s-strict s-lazy s-permissive) (e-strict e-lazy e-permissive))
              (noboth (s-in s-<) (e-in e-<))
              (noboth (s-out s-> s->! s->>) (e-out e-> e->! e->>))
              (noboth s-err e-err)
              (define (dollar-expand-maybe redirection-arg)
                (syntax-parse redirection-arg
                  [(e1:expr e ...) redirection-arg]
                  [x:keyword
                   (raise-syntax-error 'pipeline-parse
                                       "redirection arguments can't be keywords"
                                       redirection-arg)]
                  [x (dollar-expand-syntax #'x)]))
              (with-syntax
                ([bg (if (or (attribute s-bg) (attribute e-bg))
                         #'#t #'#f)]
                 [pipeline-ret (if (or (attribute s-pr) (attribute e-pr))
                                   #'#t #'#f)]
                 [env (cond [(attribute s-env-list)]
                            [(attribute e-env-list)]
                            [else #''()])]
                 [env-replace (cond [(attribute s-env-r-list)]
                                    [(attribute e-env-r-list)]
                                    [else #'#f])]
                 [lazy-timeout (or (attribute s-lazy-timeout)
                                   (attribute e-lazy-timeout)
                                   #'1)]
                 [strictness (cond
                               [(or (attribute s-strict)
                                    (attribute e-strict))
                                #''strict]
                               [(or (attribute s-lazy)
                                    (attribute e-lazy))
                                #''lazy]
                               [(or (attribute s-permissive)
                                    (attribute e-permissive))
                                #''permissive]
                               [else #''lazy])]
                 [in (cond [(attribute s-in)]
                           [(attribute e-in)]
                           [(or (attribute s-<)
                                (attribute e-<))
                            =>
                            (λ (f) (dollar-expand-maybe f))]
                           ;; TODO - respect outer macro default
                           [else #'default-pipeline-in])]
                 [out (cond [(attribute s-out)]
                            [(attribute e-out)]
                            [(or (attribute s->)
                                 (attribute e->))
                             =>
                             (λ (f) #`(list #,(dollar-expand-maybe f)
                                            'error))]
                            [(or (attribute s->!)
                                 (attribute e->!))
                             =>
                             (λ (f) #`(list #,(dollar-expand-maybe f)
                                            'truncate))]
                            [(or (attribute s->>)
                                 (attribute e->>))
                             =>
                             (λ (f) #`(list #,(dollar-expand-maybe f)
                                            'append))]
                            ;; TODO - respect outer macro default
                            [else  #'default-pipeline-out])]
                 [err (cond [(attribute s-err)]
                            [(attribute s-err)]
                            ;; TODO - respect outer macro default
                            [else #'default-pipeline-err-out])]
                 [object-to-out (if (or (attribute s-out)
                                        (attribute e-out)
                                        (attribute s->)
                                        (attribute e->)
                                        (attribute s->!)
                                        (attribute e->!)
                                        (attribute s->>)
                                        (attribute e->>))
                                    #'#t
                                    #'#f)])
                #`(rash-pipeline-splitter/start
                   run-split-pipe
                   ;; Opts here are just passed through until the last macro.
                   (
                    bg pipeline-ret
                    ;; env env-replace
                    in out err
                    strictness lazy-timeout
                    object-to-out
                    )
                   arg ...))])])])]))

(define-for-syntax (pipeline-split-loop stx def-ctx stxs names)
  (syntax-parse stx
    [() (values stxs names)]
    [((~var op (pipeline-joint def-ctx))
      (~var arg (not-pipeline-op def-ctx)) ...
      rest ...)
     (define-values (out-stx new-names)
       (dispatch-pipeline-joint (syntax/loc #'op
                                  (op arg ...))
                                def-ctx))
     (pipeline-split-loop #'(rest ...)
                          def-ctx
                          (append stxs (list out-stx))
                          (append names new-names))]))

(require racket/undefined)

(define-syntax (rash-pipeline-splitter/start stx)
  (syntax-parse stx
    ;; This is a continuation-passing style macro because I may want to run
    ;; the pipeline OR just create a first-class pipeline object.
    [(_ split-done-k opts (~var starter (pipeline-starter #f))
        (~var args (not-pipeline-op #f)) ... rest ...)
     (define def-ctx (syntax-local-make-definition-context))
     (define-values (stx1 names1) (dispatch-pipeline-starter (syntax/loc #'starter
                                                             (starter args ...))))
     (define-values (stxs2 names2)
       (pipeline-split-loop #'(rest ...) def-ctx (list stx1) names1))
     #`(split-done-k
        opts
        (let (#,@(map (λ (n) #`(#,n undefined))
                      names2))
          (list #,@stxs2)))]
    [(rps split-done-k opts (~var iargs (not-pipeline-op #f)) ...+ rest ...)
     (define iarg1 (car (syntax->list #'(iargs ...))))
     (define implicit-starter
       (datum->syntax iarg1
                      '#%shell-pipeline/default-pipeline-starter
                      iarg1))
     (syntax-parse implicit-starter
       [implicit-pipeline-starter:pipeline-starter
        #'(rps split-done-k opts
               implicit-pipeline-starter
               iargs ... rest ...)]
       [_ (raise-syntax-error
           'run-pipeline
           "Run-pipeline was used without an explicit starter and in the implicit context (the first form in the pipeline) the default is either not bound or not bound to an appropriate pipeline operator."
           #'(iargs ... rest ...)
           iarg1)])]))

#;(define-syntax (rash-pipeline-splitter/joints stx)
  (syntax-parse stx
    [(rpsj split-done-k opts (done-parts ...) ())
     #'(split-done-k opts done-parts ...)]
    [(rpsj split-done-k opts (done-parts ...)
           (op:pipeline-joint arg:not-pipeline-op ... rest ...))
     (with-syntax ([joint-segment-expression
                    (dispatch-pipeline-joint (syntax/loc #'op
                                               (op arg ...)))])
       #'(rpsj split-done-k opts
               (done-parts ... joint-segment-expression)
               (rest ...)))]))

(define-syntax (run-split-pipe stx)
  (syntax-parse stx
    [(_
      ;; opts group first
      (
       bg return-pipeline-object
       ;; env env-replace
       in out err
       strictness lazy-timeout
       object-to-out
       )
      pipe-segment-expressions-list)
     #'(rash-do-transformed-pipeline
        #:bg bg #:return-pipeline-object return-pipeline-object
        #:in in #:out out #:err err
        #:strictness strictness #:lazy-timeout lazy-timeout
        #:object-to-out object-to-out
        pipe-segment-expressions-list)]))


(define (rash-do-transformed-pipeline #:bg bg
                                      #:return-pipeline-object return-pipeline-object
                                      #:in in
                                      #:out out
                                      #:err err
                                      #:strictness strictness
                                      #:lazy-timeout lazy-timeout
                                      #:object-to-out object-to-out
                                      args)
  ;; TODO - environment extension/replacement
  (apply mp:run-mixed-pipeline
         #:bg bg #:return-pipeline-object return-pipeline-object
         #:in in #:out out #:err err
         #:strictness strictness #:lazy-timeout lazy-timeout
         #:object-to-out object-to-out
         args))

#;(define-syntax (first-class-split-pipe/start stx)
  (syntax-parse stx
    [(_ ignored-opts starter-expression joint-expression ...)
     #'(mp:composite-pipeline-member-spec
        (list starter-expression joint-expression ...))]))
#;(define-syntax (first-class-split-pipe/joint stx)
  (syntax-parse stx
    [(_ ignored-opts joint-expression ...+)
     #'(mp:composite-pipeline-member-spec
        (list joint-expression ...))]))
