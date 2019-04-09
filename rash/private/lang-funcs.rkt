#lang racket/base

(provide
 rash
 rash/wired
 with-rash-config
 splicing-with-rash-config
 run-pipeline
 run-pipeline/logic
 cd
 #%hash-braces

 (all-from-out shell/pipeline-macro)
 (all-from-out linea/line-macro)
 (all-from-out linea/defaults)
 (all-from-out "top-level-print.rkt")

 make-rash-reader-submodule
 define-rash-module-begin
 (for-syntax
  make-rash-transformer
  ))

(module+ experimental
  (provide (for-syntax rash-template-escaper)))

(module+ for-repl
  (provide
   splicing-with-pipeline-config
   run-pipeline
   run-pipeline/logic
   ))


(require
 (rename-in shell/pipeline-macro
            [run-pipeline run-pipeline/no-line-macro]
            [run-pipeline/logic run-pipeline/logic/no-line-macro])
 racket/splicing
 racket/string
 racket/port
 "cd.rkt"
 "top-level-print.rkt"
 linea/line-macro
 linea/defaults
 linea/read
 linea/read-syntax-string
 (only-in shell/private/pipeline-macro-parse splicing-with-pipeline-config)
 syntax/parse
 syntax/wrap-modbeg

 (for-syntax
  syntax/wrap-modbeg
  syntax/keyword
  racket/base
  syntax/parse
  linea/line-macro
  linea/line-macro-prop
  linea/defaults
  linea/read
  linea/read-syntax-string
  udelim
  racket/port
  syntax/strip-context
  shell/private/misc-utils

  (for-syntax
   linea/line-macro
   linea/defaults
   linea/read
   linea/read-syntax-string
   syntax/wrap-modbeg
   racket/base
   syntax/parse
   syntax/keyword
   "template-escape-detect.rkt"
   shell/private/misc-utils
   )))

(define-line-macro run-pipeline
  (λ (stx)
    (syntax-parse stx
      [(_ arg ...)
       #'(run-pipeline/no-line-macro arg ...)])))
(define-line-macro run-pipeline/logic
  (λ (stx)
    (syntax-parse stx
      [(_ arg ...)
       #'(run-pipeline/logic/no-line-macro arg ...)])))

(define default-output-port-transformer (λ (p) (string-trim (begin0 (port->string p)
                                                              (close-input-port p)))))

(module keyword-table racket/base
  (provide rash-keyword-table)
  (require syntax/keyword)
  (define rash-keyword-table
    (list (list '#:in check-expression)
          (list '#:out check-expression)
          (list '#:err check-expression)
          (list '#:starter check-expression)
          (list '#:line-macro check-expression))))
(require (for-syntax (submod "." keyword-table))
         (for-meta 2 (submod "." keyword-table)))

(define-syntax (rash-expressions-begin stx)
  (syntax-parse stx
    [(_ (input output err-output default-starter line-macro) e ...+)
     #`(splicing-let ([in-eval input]
                      [out-eval output]
                      [err-eval err-output])
         (splicing-with-rash-config
          #:line-macro line-macro
          #:starter default-starter
          #:in in-eval
          #:out out-eval
          #:err err-eval
          e ...))]))

(define-for-syntax (with-rash-config* stx lm-parameterizer p-parameterizer)
  (syntax-parse stx
    [(orig-macro
      (~or
       (~optional (~seq #:context context))
       (~optional (~seq #:in in:expr))
       (~optional (~seq #:out out:expr))
       (~optional (~seq #:err err:expr))
       (~optional (~seq #:starter starter:pipeline-starter))
       (~optional (~seq #:line-macro line-macro:line-macro)))
      ...
      body:expr ...+)
     (define context-id
       (or (attribute context)
           (let ([cs (map (λ (x) (datum->syntax x '#%app #'orig-macro))
                          (syntax->list #'(body ...)))])
             (unless (for/and ([x (cdr cs)])
                       (bound-identifier=? (car cs) x))
               (raise-syntax-error
                'with-rash-config
                "Multiple body forms were given with different scoping information, so there is not a clear choice of info to bind the default line macro and pipeline starter to."
                stx))
             (car cs))))
     (with-syntax ([lm-param-form (if (attribute line-macro)
                                      #`(#,lm-parameterizer
                                         #:context #,context-id
                                         line-macro)
                                      #'(begin))]
                   [p-param-in (if (attribute in)
                                   #'(#:in in)
                                   #'())]
                   [p-param-out (if (attribute out)
                                    #'(#:out out)
                                    #'())]
                   [p-param-err (if (attribute err)
                                    #'(#:err err)
                                    #'())]
                   [p-param-starter (if (attribute starter)
                                        #'(#:starter starter)
                                        #'())])
       (with-syntax ([p-param-form (if (or (attribute in)
                                           (attribute out)
                                           (attribute err)
                                           (attribute starter))
                                       #`(#,p-parameterizer
                                          #:context #,context-id
                                          #,@#'p-param-in
                                          #,@#'p-param-out
                                          #,@#'p-param-err
                                          #,@#'p-param-starter)
                                       #'(begin))])
         #`(#,@#'lm-param-form
            (#,@#'p-param-form
             body ...))))]))

(define-syntax (with-rash-config stx)
  (with-rash-config* stx
    #'with-default-line-macro
    #'with-pipeline-config))
(define-syntax (splicing-with-rash-config stx)
  (with-rash-config* stx
    #'splicing-with-default-line-macro
    #'splicing-with-pipeline-config))

(define-syntax (#%hash-braces stx)
  (syntax-parse stx
    [(_ body:expr)
     #'(splicing-with-rash-config #:in (open-input-string "")
                                  #:out (λ (p) (string-trim (begin0 (port->string p)
                                                              (close-input-port p))))
                                  #:err stderr-capture-redirect
                                  ;#:starter =unix-pipe=
                                  ;#:line-macro run-pipeline
                                  body)]))

#|
TODO

The define-rash-module-begin and make-reader-submodule stuff is really an
intermediate step to better configurability.

I want a form to easily define a new #lang based on rash with custom options for
the defaults and the reader.  Something like this:

```
#lang racket/base
(require rash)
(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (all-from-out rash)
         (except-out (all-defined-out)
                     my-mb)
         (rename-out [my-mb #%module-begin])
         )
(rash-hash-lang-setup
 #:module-begin-name my-mb
 #:default-starter =object-pipe=
 #:rash-readtable (modify-readtable-somehow basic-rash-readtable)
 ...
 )
```

And then be able to use the module path for that module as a #lang.

Also it might be nice for #lang rash to take arguments that affect it somehow.
But how can it be done in a way that let those arguments affect the reader?
|#

(define-syntax (make-rash-reader-submodule stx)
  (syntax-parse stx
    [(_ this-module-path)
     #'(begin
         (module reader syntax/module-reader
           this-module-path
           #:read-syntax linea-read-syntax
           #:read linea-read
           #:info (λ (key defval default)
                    (case key
                      [(color-lexer)
                       (dynamic-require 'syntax-color/default-lexer
                                        'default-lexer)]
                      [(drracket:submit-predicate)
                       (dynamic-require 'rash/private/drracket-submit-predicate
                                        'submit-predicate)]
                      [else (default key defval)]))
           (require linea/read)))]))

(define (rash-top-level-print . vs)
  (for ([v vs])
    (let ([str ((current-rash-top-level-print-formatter) v)])
      (when (not (equal? str ""))
        (displayln str)))))
(define-syntax (wrap-rash-top-level-print stx)
  (syntax-parse stx
    [(_ e ...)
     #'(call-with-values (λ () e ...) rash-top-level-print)]))

(define-syntax (define-rash-module-begin stx)
  (syntax-parse stx
    [(_ rmb-name make-mb-arg ...)
     (define-values (tab rest-stx)
       (parse-keyword-options #'(make-mb-arg ...)
                              (list*
                               (list '#:this-module-path check-expression)
                               (list '#:top-level-wrap check-expression)
                               rash-keyword-table)
                              #:context stx
                              #:no-duplicates? #t))
     (syntax-parse rest-stx
       [() (void)]
       [else (raise-syntax-error 'make-rash-module-begin-transformer "unexpected arguments" rest-stx)])
     (with-syntax ([this-mod-path (opref tab '#:this-module-path
                                         #'(raise-syntax-error
                                            'make-rash-module-begin-transformer
                                            "expected #:this-module-path argument."))]
                   [top-level-wrap (opref tab '#:top-level-wrap #'wrap-rash-top-level-print)]
                   [mk-input (opref tab '#:in #'(current-input-port))]
                   [mk-output (opref tab '#:out #'(current-output-port))]
                   [mk-err-output (opref tab '#:err #'(current-error-port))]
                   [mk-default-starter (opref tab '#:starter
                                              #'=unix-pipe=)]
                   [mk-default-line-macro (opref tab '#:line-macro
                                                 #'run-pipeline)])
       #'(begin
           (define-syntax wrapping-module-begin-for-rash
             (make-wrapping-module-begin #'top-level-wrap))
           (define-syntax rmb-name
             (syntax-parser
               [(_ arg (... ...))
                #'(wrapping-module-begin-for-rash
                   (module configure-runtime racket/base
                     (require linea/read
                              rash/private/lang-funcs
                              this-mod-path)
                     (current-read-interaction
                      (λ (src in)
                        (let ([stx (linea-read-syntax src in)])
                          (if (eof-object? stx)
                              stx
                              (syntax-parse stx
                                [e #'(rash-expressions-begin
                                      (mk-input
                                       mk-output
                                       mk-err-output
                                       mk-default-starter
                                       mk-default-line-macro)
                                      e)])))))
                     (current-print rash-top-level-print))
                   (rash-expressions-begin (mk-input
                                            mk-output
                                            mk-err-output
                                            mk-default-starter
                                            mk-default-line-macro)
                                           arg (... ...)))]))))]))

(begin-for-syntax

  (define-syntax (make-rash-transformer stx)
    (syntax-parse stx
      [(_ make-transformer-arg ...)
       (define-values (tab rest-stx)
         (parse-keyword-options #'(make-transformer-arg ...)
                                rash-keyword-table
                                #:context stx
                                #:no-duplicates? #t))
       (syntax-parse rest-stx
         [() (void)]
         [else (raise-syntax-error
                'make-rash-transformer
                "unexpected arguments"
                rest-stx)])
       (with-syntax ([mk-input (opref tab '#:in #'(open-input-string ""))]
                     [mk-output (opref tab '#:out #'default-output-port-transformer)]
                     [mk-err-output (opref tab '#:err #'stderr-capture-redirect)]
                     ;; TODO - make it possible for these to inherit
                     [mk-default-starter (opref tab '#:starter
                                                #'=unix-pipe=)]
                     [mk-line-macro (opref tab '#:line-macro
                                           #'run-pipeline)])
         #'(λ (stx)
             (syntax-parse stx
               [(rash tx-arg (... ...))
                (define-values (tab rest-stx)
                  (parse-keyword-options #'(tx-arg (... ...))
                                         rash-keyword-table
                                         #:context stx
                                         #:no-duplicates? #t))

                (with-syntax ([input
                               (opref tab '#:in (quote-syntax mk-input))]
                              [output
                               (opref tab '#:out (quote-syntax mk-output))]
                              [err-output
                               (opref tab '#:err (quote-syntax mk-err-output))]
                              [default-starter
                                (opref tab '#:starter
                                       (quote-syntax mk-default-starter))]
                              [line-macro
                               (opref tab '#:line-macro
                                      (quote-syntax mk-line-macro))])
                  (syntax-parse rest-stx
                    [(arg:str (... ...))
                     (with-syntax ([parsed-rash-code (linea-read-syntax-string rest-stx)])
                       #'(rash-expressions-begin (input output err-output
                                                        default-starter
                                                        line-macro)
                                                 parsed-rash-code))]))])))]))
  )

(define-syntax rash
  (make-rash-transformer))
(define-syntax rash/wired
  (make-rash-transformer #:in (current-input-port)
                         #:out (current-output-port)
                         #:err (current-error-port)))

(begin-for-syntax
  (define-syntax rash-template-escaper
    (template-escape-struct
     (λ (stx)
       (syntax-parse stx
         [(_ arg ...)
          (with-syntax ([parsed-rash-code (linea-read-syntax-string #'(arg ...))])
            #'(rash-expressions-begin
               ((open-input-string "")
                default-output-port-transformer
                shared-string-port-redirect)
               parsed-rash-code))])))))

