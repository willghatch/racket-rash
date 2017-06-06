#lang racket/base

(provide
 rash-line-parse
 rash-read-and-line-parse
 default-output-port-transformer
 rash-pipeline-opt-hash
 (for-syntax opref)
 &bg &pipeline-ret &env &env-replace &in &< &out &> &>! &>> &err
 )

(module+ for-public
  (provide
   &bg &pipeline-ret &env &env-replace &in &< &out &> &>! &>> &err
   rash-do-pipeline
   ))

(require
 "read-funcs.rkt"
 syntax/parse
 racket/stxparam
 racket/string
 racket/port
 shell/mixed-pipeline
 "pipeline-operator-implicit.rkt"
 "pipeline-operators.rkt"
 "pipeline-operator-transform.rkt"
 (for-syntax
  racket/base
  syntax/parse
  racket/stxparam-exptime
  syntax/keyword
  racket/dict
  "pipeline-operator-detect.rkt"
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
;; TODO - add one for strict/lazy/permissive status checking


(begin-for-syntax
  (define-syntax-class not-opt
    #:literals (&bg &pipeline-ret &env &env-replace &in &out &err &< &> &>! &>>)
    (pattern (~not (~or &bg &pipeline-ret &env &env-replace &in &out &err &< &> &>! &>>)))))

(define (rash-read-and-line-parse src in)
  (let ([stx (rash-read-syntax src in)])
    (if (eof-object? stx)
        stx
        (syntax-parse stx
          [e #'(rash-line-parse ((current-input-port)
                                 (current-output-port)
                                 (current-error-port))
                                e)]))))

(define-syntax (rash-line-parse stx)
  (define-syntax-class not-newline
    (pattern (~and (~not (~literal %%rash-newline-symbol))
                   (~not ((~literal %%rash-racket-line) e ...)))))
  (define-syntax-class rash-newline
    (pattern (~or (~literal %%rash-newline-symbol)
                  ((~literal %%rash-racket-line) e ...))))
  (syntax-parse stx
    [(rlp (in out err) arg ...)
     (with-syntax ([ioe #'(in out err)])
       (syntax-parse #'(arg ...)
         #:datum-literals (%%rash-newline-symbol %%rash-racket-line %%rash-line-start)
         [((%%rash-line-start arg ...) post ...+)
          #'(begin (rash-pipeline-splitter ioe arg ...)
                   (rlp ioe post ...))]
         [((%%rash-line-start arg ...))
          #'(rash-pipeline-splitter ioe arg ...)]
         [((%%rash-racket-line arg ...) post ...+)
          #'(begin arg ...
                   (rlp ioe post ...))]
         [((%%rash-racket-line arg ...))
          #'(begin arg ...)]
         [() #'(void)]))]))

;; To avoid passing more syntax through layers of macros
(define rash-pipeline-opt-hash (make-parameter (hash)))
(define (ropt key)
  (hash-ref (rash-pipeline-opt-hash) key))

(define-syntax (rash-pipeline-splitter stx)
  ;; Parse out the beginning/ending whole-pipeline options, then
  ;; pass the pipeline specs to other macros to deal with.
  (syntax-parse stx
    [(_ (outer-in outer-out outer-err) rash-arg ...)
     (syntax-parse #'(rash-arg ...)
       #:literals (&bg &pipeline-ret &env &env-replace &in &out &err &< &> &>! &>>)
       [((~or (~optional (~and s-bg &bg) #:name "&bg option")
              (~optional (~and s-pr &pipeline-ret) #:name "&pipeline-ret option")
              (~optional (~seq &env s-env-list:expr) #:name "&env option")
              (~optional (~seq &env-replace s-env-r-list:expr) #:name "&env-replace option")
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
          #:literals (&bg &pipeline-ret &env &env-replace &in &out &err &< &> &>! &>>)
          [((~or (~optional (~and e-bg &bg) #:name "&bg option")
                 (~optional (~and e-pr &pipeline-ret) #:name "&pipeline-ret option")
                 (~optional (~seq e-env-list:expr &env) #:name "&env option")
                 (~optional (~seq e-env-r-list:expr &env-replace) #:name "&env-replace option")
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
                                      'in #,(cond [(attribute s-in)]
                                                  [(attribute e-in)]
                                                  [(attribute s-<) #`(quote #,(attribute s-<))]
                                                  [(attribute e-<) #`(quote #,(attribute e-<))]
                                                  ;; TODO - respect outer macro default
                                                  [else #'outer-in])
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
                                                   [else  #'outer-out])
                                      'err #,(cond [(attribute s-err)]
                                                   [(attribute s-err)]
                                                   ;; TODO - respect outer macro default
                                                   [else #'outer-err])
                                      )])
                  (rash-pipeline-splitter/start rash-pipeline-splitter/done/do arg ...))])])])]))

(define-syntax (rash-pipeline-splitter/start stx)
  (syntax-parse stx
    [(_ done-macro starter:pipe-starter-op args:not-pipeline-op ... rest ...)
     #'(rash-pipeline-splitter/rest done-macro ([starter args ...]) (rest ...))]
    [(rps done-macro iargs:not-pipeline-op ...+ rest ...)
     #`(rps done-macro
            #,(hash-ref implicit-pipe-starter-hash
                        {syntax-parameter-value #'implicit-pipe-starter-key})
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
        (rash-transform-starter-segment starter startarg ...)
        (rash-transform-joiner-segment joiner joinarg ...) ...)]))

(define-for-syntax (opref table key default)
  ;; For getting options with default out of a
  ;; parses-keyword-options result hash
  (with-handlers ([(λ _ #t) (λ (e) default)])
    (cadr (dict-ref table key))))

(define-syntax (rash-do-pipeline stx)
  (syntax-parse stx
    [(_ arg ...+)
     (define-values (tab rest-stx)
       (parse-keyword-options #'(arg ...)
                              (list (list '#:in check-expression)
                                    (list '#:< check-expression)
                                    (list '#:out check-expression)
                                    (list '#:> check-expression)
                                    (list '#:>! check-expression)
                                    (list '#:>> check-expression)
                                    (list '#:err check-expression)
                                    ;; TODO - name?
                                    (list '#:return-pipeline-object check-expression)
                                    (list '#:bg check-expression)
                                    (list '#:env check-expression)
                                    (list '#:env-replace check-expression)
                                    )
                              #:no-duplicates? #t
                              #:incompatible '((#:in #:<) (#:out #:> #:>! #:>>))))
     (with-syntax ([input (cond [(opref tab '#:< #f)
                                 => (λ (s) (syntax-case s () [in #'(quote in)]))]
                                [(opref tab '#:in #'(open-input-string ""))])]
                   [output (cond [(opref tab '#:> #f)
                                  => (λ (s) (syntax-case s ()
                                              [out #'(list (quote out) 'error)]))]
                                 [(opref tab '#:>! #f)
                                  => (λ (s) (syntax-case s ()
                                              [out #'(list (quote out) 'truncate)]))]
                                 [(opref tab '#:>> #f)
                                  => (λ (s) (syntax-case s ()
                                              [out #'(list (quote out) 'append)]))]
                                 [(opref tab '#:out #'default-output-port-transformer)])]
                   [err-output (opref tab '#:err #''string-port)]
                   [bg (opref tab '#:bg #'#f)]
                   [return-pipeline-object (opref tab '#:return-pipeline-object #'#f)]
                   [env (opref tab '#:env #'#f)]
                   [replace-env (opref tab '#:replace-env #'#f)])
       (syntax-parse rest-stx
         [([startop:pipe-starter-op startarg ...] [joinop:pipe-joiner-op joinarg ...] ...)
          #'(rash-do-transformed-pipeline
               #:bg bg #:return-pipeline-object return-pipeline-object
               ;#:env env #:replace-env replace-env
               #:in input #:out output #:err err-output
               (rash-transform-starter-segment startop startarg ...)
               (rash-transform-joiner-segment joinop joinarg ...)
               ...)]))]))

(define default-output-port-transformer (λ (p) (string-trim (port->string p))))

(define (rash-do-transformed-pipeline #:bg bg
                                      #:return-pipeline-object return-pipeline-object
                                      #:in in
                                      #:out out
                                      #:err err
                                      . args)
  ;; TODO - environment extension/replacement
  (run-pipeline args #:bg bg #:return-pipeline-object return-pipeline-object
                #:in in #:out out #:err err))
