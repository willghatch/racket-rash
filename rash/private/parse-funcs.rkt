#lang racket/base

(provide
 rash-line-parse
 rash-read-and-line-parse
 rash-set-defaults
 default-output-port-transformer
 rash-pipeline-opt-hash
 &bg &pipeline-ret &env &env-replace &in &< &out &> &>! &>> &err
 )

(module+ for-public
  (provide
   &bg &pipeline-ret &env
   ;&env-replace
   &in &< &out &> &>! &>> &err
   &strict &permissive &lazy &lazy-timeout
   rash-line-or-line-macro
   rash-run-pipeline
   ))

(require
 "read-funcs.rkt"
 syntax/parse
 racket/stxparam
 racket/splicing
 racket/string
 racket/port
 shell/mixed-pipeline
 "pipeline-operator-default.rkt"
 "pipeline-operators.rkt"
 "pipeline-operator-transform.rkt"
 (for-syntax
  racket/base
  syntax/parse
  racket/stxparam-exptime
  syntax/keyword
  racket/dict
  "pipeline-operator-detect.rkt"
  "line-macro-detect.rkt"
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

(define-syntax-parameter rash-default-in
  (λ (stx) (raise-syntax-error 'rash-default-in
                               "Internal error - default used where none is set")))
(define-syntax-parameter rash-default-out
  (λ (stx) (raise-syntax-error 'rash-default-out
                               "Internal error - default used where none is set")))
(define-syntax-parameter rash-default-err-out
  (λ (stx) (raise-syntax-error 'rash-default-err-out
                               "Internal error - default used where none is set")))

(define-syntax (rash-set-defaults stx)
  (syntax-parse stx
    [(_ (in out err) body ...)
     #'(splicing-syntax-parameterize
           ([rash-default-in (λ (stx) #'in)]
            [rash-default-out (λ (stx) #'out)]
            [rash-default-err-out (λ (stx) #'err)])
         body ...)]))

(define (rash-read-and-line-parse src in)
  (let ([stx (rash-read-syntax src in)])
    (if (eof-object? stx)
        stx
        (syntax-parse stx
          [e #'(rash-set-defaults ((current-input-port)
                                   (current-output-port)
                                   (current-error-port))
                                  (rash-line-parse e))]))))

(define-syntax (rash-line-parse stx)
  (syntax-parse stx
    [(rlp arg ...)
     (syntax-parse #'(arg ...)
       #:datum-literals (%%rash-racket-line %%rash-line-start)
       [((%%rash-line-start arg ...) post ...+)
        #'(begin (rash-line-or-line-macro arg ...)
                 (rlp post ...))]
       [((%%rash-line-start arg ...))
        #'(rash-line-or-line-macro arg ...)]
       [((%%rash-racket-line arg ...) post ...+)
        #'(begin arg ...
                 (rlp post ...))]
       [((%%rash-racket-line arg ...))
        #'(begin arg ...)]
       [() #'(void)])]))

(define-syntax (rash-line-or-line-macro stx)
  ;; detect line macros and apply them, or transform into pipeline
  (syntax-parse stx
    [(_ arg1:line-macro arg ...)
     (rash-line-macro-transform #'(arg1 arg ...))]
    [(_ arg ...)
     #'(rash-pipeline-splitter arg ...)]))

(define-syntax (rash-run-pipeline stx)
  (syntax-parse stx
    [(_ arg ...)
     #'(rash-pipeline-splitter arg ...)]))

;; To avoid passing more syntax through layers of macros
(define rash-pipeline-opt-hash (make-parameter (hash)))
(define (ropt key)
  (hash-ref (rash-pipeline-opt-hash) key))

(define-syntax (rash-pipeline-splitter stx)
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
                        'rash-pipeline-splitter
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
              #`(parameterize ([rash-pipeline-opt-hash
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
                                                  [(attribute s-<) #`(quote #,(attribute s-<))]
                                                  [(attribute e-<) #`(quote #,(attribute e-<))]
                                                  ;; TODO - respect outer macro default
                                                  [else #'rash-default-in])
                                      'out #,(cond [(attribute s-out)]
                                                   [(attribute e-out)]
                                                   [(attribute s->) #`(list (quote #,(attribute s->))
                                                                            'error)]
                                                   [(attribute e->) #`(list (quote #,(attribute e->))
                                                                            'error)]
                                                   [(attribute s->!) #`(list (quote #,(attribute s->!))
                                                                             'truncate)]
                                                   [(attribute e->!) #`(list (quote #,(attribute e->!))
                                                                             'truncate)]
                                                   [(attribute s->>) #`(list (quote #,(attribute s->>))
                                                                             'append)]
                                                   [(attribute e->>) #`(list (quote #,(attribute e->>))
                                                                             'append)]
                                                   ;; TODO - respect outer macro default
                                                   [else  #'rash-default-out])
                                      'err #,(cond [(attribute s-err)]
                                                   [(attribute s-err)]
                                                   ;; TODO - respect outer macro default
                                                   [else #'rash-default-err-out])
                                      )])
                  (rash-pipeline-splitter/start rash-pipeline-splitter/done/do arg ...))])])])]))

(define-syntax (rash-pipeline-splitter/start stx)
  (syntax-parse stx
    [(_ done-macro starter:pipe-starter-op args:not-pipeline-op ... rest ...)
     #'(rash-pipeline-splitter/rest done-macro ([starter args ...]) (rest ...))]
    [(rps done-macro iargs:not-pipeline-op ...+ rest ...)
     #`(rps done-macro
            #,(get-default-pipeline-starter)
            iargs ... rest ...)]))

(define-syntax (rash-pipeline-splitter/rest stx)
  (syntax-parse stx
    [(rpsr done-macro (done-parts ...) ())
     #'(done-macro done-parts ...)]
    [(rpsr done-macro
           (done-parts ...)
           (op:pipe-joiner-op arg:not-pipeline-op ... rest ...))
     #'(rpsr done-macro (done-parts ... [op arg ...]) (rest ...))]))

(define-syntax (rash-pipeline-splitter/done/do stx)
  (syntax-parse stx
    [(_ (starter startarg ...) (joiner joinarg ...) ...)
     #'(rash-do-transformed-pipeline
        #:bg (ropt 'bg) #:return-pipeline-object (ropt 'pipeline-ret)
        #:in (ropt 'in) #:out (ropt 'out) #:err (ropt 'err)
        #:strictness (ropt 'strictness) #:lazy-timeout (ropt 'lazy-timeout)
        (rash-transform-starter-segment starter startarg ...)
        (rash-transform-joiner-segment joiner joinarg ...) ...)]))


(define default-output-port-transformer (λ (p) (string-trim (port->string p))))

(define (rash-do-transformed-pipeline #:bg bg
                                      #:return-pipeline-object return-pipeline-object
                                      #:in in
                                      #:out out
                                      #:err err
                                      #:strictness strictness
                                      #:lazy-timeout lazy-timeout
                                      . args)
  ;; TODO - environment extension/replacement
  (apply run-pipeline #:bg bg #:return-pipeline-object return-pipeline-object
         #:in in #:out out #:err err
         #:strictness strictness #:lazy-timeout lazy-timeout
         args))
