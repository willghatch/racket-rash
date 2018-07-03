#lang racket/base

(module+ for-public
  (provide
   &bg &pipeline-ret &env
   &in &< &out &> &>! &>> &err
   &strict &permissive &lazy &lazy-timeout
   pipeline-start-segment
   pipeline-joint-segment
   run-pipeline
   with-pipeline-parameters
   splicing-with-pipeline-parameters
   ))

(provide
 pipeline-start-segment
 pipeline-joint-segment
 run-pipeline
 with-pipeline-parameters
 splicing-with-pipeline-parameters
 rash-pipeline-opt-hash
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
 "pipeline-operator-transform.rkt"
 "../utils/bourne-expansion-utils.rkt"
 (for-syntax
  racket/base
  syntax/parse
  racket/stxparam-exptime
  syntax/keyword
  racket/dict
  "pipeline-operator-detect.rkt"
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
  (syntax-parser [_ #'(λ (x) (string-trim (port->string x)))]))
(define-syntax-parameter default-pipeline-err-out
  (syntax-parser [_ #''string-port]))

(define-syntax (with-pipeline-parameters* stx)
  (syntax-parse stx
    [(_ parameterization-form
        (~or (~optional (~seq #:in in))
             (~optional (~seq #:out out))
             (~optional (~seq #:err err))
             (~optional (~seq #:starter starter:pipeline-starter)))
        ...
        body:expr ...+)
     (let* ([set-in (and (attribute in)
                         #'(default-pipeline-in (λ (stx) (quote-syntax in))))]
            [set-out (and (attribute out)
                          #'(default-pipeline-out (λ (stx) (quote-syntax out))))]
            [set-err (and (attribute err)
                          #'(default-pipeline-err-out (λ (stx) (quote-syntax err))))]
            [set-starter (and (attribute starter)
                              #'(default-pipeline-starter (quote-syntax starter)))]
            [parameterizations
             #`(#,@(filter (λ(x)x) (list set-in set-out set-err set-starter)))])
       #`(parameterization-form
          #,parameterizations
          body ...))]))
(begin-for-syntax
  (define-splicing-syntax-class kw-opt
    (pattern (~seq kw:keyword val:expr))))
(define-syntax (with-pipeline-parameters stx)
  (syntax-parse stx
    [(_ opt:kw-opt ... body:expr ...+)
     (syntax-parse #'(opt ...)
       [(((~or (~optional (~seq #:in in))
               (~optional (~seq #:out out))
               (~optional (~seq #:err err))
               (~optional (~seq #:starter starter:pipeline-starter))))
         ...)
        #`(with-pipeline-parameters*
            syntax-parameterize
            #,@(apply append (map syntax->list
                                  (syntax->list #'(opt ...))))
            body ...)])]))
(define-syntax (splicing-with-pipeline-parameters stx)
  (syntax-parse stx
    [(_ opt:kw-opt ... body:expr ...+)
     (syntax-parse #'(opt ...)
       [(((~or (~optional (~seq #:in in))
               (~optional (~seq #:out out))
               (~optional (~seq #:err err))
               (~optional (~seq #:starter starter:pipeline-starter))))
         ...)
        #`(with-pipeline-parameters*
            splicing-syntax-parameterize
            #,@(apply append (map syntax->list
                                  (syntax->list #'(opt ...))))
            body ...)])]))

(define-syntax (pipeline-start-segment stx)
  (syntax-parse stx
    [(_ arg ...+)
     #'(rash-pipeline-splitter/start first-class-split-pipe/start arg ...)]))
(define-syntax (pipeline-joint-segment stx)
  (syntax-parse stx
    [(_ arg ...+)
     #'(rash-pipeline-splitter/joints first-class-split-pipe/joint () (arg ...))]))

(define-syntax (run-pipeline stx)
  (syntax-parse stx
    [(_ arg ...)
     #'(run-pipeline/split arg ...)]))

;; To avoid passing more syntax through layers of macros
(define rash-pipeline-opt-hash (make-parameter (hash)))
(define (ropt key)
  (hash-ref (rash-pipeline-opt-hash) key))

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
              #`(parameterize
                    ([rash-pipeline-opt-hash
                      (hash 'bg #,(if (or (attribute s-bg) (attribute e-bg))
                                      #'#t #'#f)
                            'pipeline-ret #,(if (or (attribute s-pr) (attribute e-pr))
                                                #'#t #'#f)
                            'env #,(cond [(attribute s-env-list)]
                                         [(attribute e-env-list)]
                                         [else #''()])
                            'env-replace #,(cond [(attribute s-env-r-list)]
                                                 [(attribute e-env-r-list)]
                                                 [else #'#f])
                            'lazy-timeout #,(or (attribute s-lazy-timeout)
                                                (attribute e-lazy-timeout)
                                                #'1)
                            'strictness #,(cond
                                            [(or (attribute s-strict)
                                                 (attribute e-strict))
                                             #''strict]
                                            [(or (attribute s-lazy)
                                                 (attribute e-lazy))
                                             #''lazy]
                                            [(or (attribute s-permissive)
                                                 (attribute e-permissive))
                                             #''permissive]
                                            [else #''lazy])
                            'in #,(cond [(attribute s-in)]
                                        [(attribute e-in)]
                                        [(or (attribute s-<)
                                             (attribute e-<))
                                         =>
                                         (λ (f) (dollar-expand-maybe f))]
                                        ;; TODO - respect outer macro default
                                        [else #'default-pipeline-in])
                            'out #,(cond [(attribute s-out)]
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
                                         [else  #'default-pipeline-out])
                            'err #,(cond [(attribute s-err)]
                                         [(attribute s-err)]
                                         ;; TODO - respect outer macro default
                                         [else #'default-pipeline-err-out])
                            'object-to-out #,(if (or (attribute s-out)
                                                     (attribute e-out)
                                                     (attribute s->)
                                                     (attribute e->)
                                                     (attribute s->!)
                                                     (attribute e->!)
                                                     (attribute s->>)
                                                     (attribute e->>))
                                                 #'#t
                                                 #'#f)
                            )])
                  (rash-pipeline-splitter/start run-split-pipe
                                                arg ...))])])])]))

(define-syntax (rash-pipeline-splitter/start stx)
  (syntax-parse stx
    [(_ do-macro starter:pipeline-starter args:not-pipeline-op ... rest ...)
     #'(rash-pipeline-splitter/joints do-macro ([starter args ...]) (rest ...))]
    [(rps do-macro iargs:not-pipeline-op ...+ rest ...)
     #`(rps do-macro
            #,(syntax-parameter-value #'default-pipeline-starter)
            iargs ... rest ...)]))

(define-syntax (rash-pipeline-splitter/joints stx)
  (syntax-parse stx
    [(rpsj do-macro (done-parts ...) ())
     #'(do-macro done-parts ...)]
    [(rpsj do-macro (done-parts ...)
           (op:pipeline-joint arg:not-pipeline-op ... rest ...))
     #'(rpsj do-macro (done-parts ... [op arg ...]) (rest ...))]))

(define-syntax (run-split-pipe stx)
  (syntax-parse stx
    [(_ (starter startarg ...) (joint joinarg ...) ...)
     #'(rash-do-transformed-pipeline
        #:bg (ropt 'bg) #:return-pipeline-object (ropt 'pipeline-ret)
        #:in (ropt 'in) #:out (ropt 'out) #:err (ropt 'err)
        #:strictness (ropt 'strictness) #:lazy-timeout (ropt 'lazy-timeout)
        #:object-to-out (ropt 'object-to-out)
        (transform-starter-segment starter startarg ...)
        (transform-joint-segment joint joinarg ...) ...)]))


(define (rash-do-transformed-pipeline #:bg bg
                                      #:return-pipeline-object return-pipeline-object
                                      #:in in
                                      #:out out
                                      #:err err
                                      #:strictness strictness
                                      #:lazy-timeout lazy-timeout
                                      #:object-to-out object-to-out
                                      . args)
  ;; TODO - environment extension/replacement
  (apply mp:run-mixed-pipeline
         #:bg bg #:return-pipeline-object return-pipeline-object
         #:in in #:out out #:err err
         #:strictness strictness #:lazy-timeout lazy-timeout
         #:object-to-out object-to-out
         args))

(define-syntax (first-class-split-pipe/start stx)
  (syntax-parse stx
    [(_ (starter startarg ...) (joint joinarg ...) ...)
     #'(mp:composite-pipeline-member-spec
        (list (transform-starter-segment starter startarg ...)
              (transform-joint-segment joint joinarg ...) ...))]))
(define-syntax (first-class-split-pipe/joint stx)
  (syntax-parse stx
    [(_ (joint joinarg ...) ...+)
     #'(mp:composite-pipeline-member-spec
        (list (transform-joint-segment joint joinarg ...) ...))]))
