#lang racket/base

(provide
 rash
 rash/wired
 pipeline-line-macro
 cd

 (all-from-out shell/pipeline-macro)
 (all-from-out linea/line-macro)

 (for-syntax
  make-rash-module-begin-transformer
  make-rash-transformer
  ))

(module+ experimental
  (provide (for-syntax rash-template-escaper)))

(module+ for-module-begin
  (provide (for-syntax make-rash-module-begin-transformer)))

(module+ for-repl
  (provide
   rash-set-defaults
   pipeline-line-macro
   ))


(require
 racket/splicing
 racket/string
 racket/port
 "cd.rkt"
 linea/line-macro
 linea/line-parse
 linea/read
 shell/pipeline-macro
 (only-in shell/private/pipeline-macro-parse rash-set-defaults)
 syntax/parse

 (for-syntax
  syntax/keyword
  racket/base
  syntax/parse
  linea/read
  shell/private/misc-utils

  (for-syntax
   racket/base
   syntax/parse
   syntax/keyword
   "template-escape-detect.rkt"
   shell/private/misc-utils
   )))

(define-line-macro pipeline-line-macro
  (λ (stx)
    (syntax-parse stx
      [(_ arg ...)
       #'(run-pipeline arg ...)])))

(define default-output-port-transformer (λ (p) (string-trim (port->string p))))

(module keyword-table racket/base
  (provide rash-keyword-table)
  (require syntax/keyword)
  (define rash-keyword-table
    (list (list '#:in check-expression)
          (list '#:out check-expression)
          (list '#:err check-expression)
          (list '#:default-starter check-expression)
          (list '#:default-line-macro check-expression))))
(require (for-syntax (submod "." keyword-table))
         (for-meta 2 (submod "." keyword-table)))

(define-syntax (rash-expressions-begin stx)
  (syntax-parse stx
    [(_ (input output err-output default-starter line-macro) e ...+)
     #`(splicing-let ([in-eval input]
                      [out-eval output]
                      [err-eval err-output])
         (splicing-syntax-parameterize ([default-pipeline-starter default-starter]
                                        [default-line-macro line-macro])
           (rash-set-defaults
            (in-eval out-eval err-eval)
            (linea-line-parse e ...))))]))

(define ((mk-rash-read-interaction default-starter-stx default-line-macro-stx) src in)
  ;; TODO - This really should be as close as using repl.rkt as possible
  (let ([stx (linea-read-syntax src in)])
    (if (eof-object? stx)
        stx
        (syntax-parse stx
          [e #`(rash-expressions-begin
                ((current-input-port)
                 (current-output-port)
                 (current-error-port)
                 ;; TODO - make this configurable along with the one in the #lang body
                 #,default-starter-stx
                 #,default-line-macro-stx)
                e)]))))

(begin-for-syntax
  (define-syntax (make-rash-module-begin-transformer stx)
    (syntax-parse stx
      [(_ make-module-begin-arg ...)
       (define-values (tab rest-stx)
         (parse-keyword-options #'(make-module-begin-arg ...)
                                rash-keyword-table
                                #:context stx
                                #:no-duplicates? #t))
       (syntax-parse rest-stx
         [() (void)]
         [else (raise-syntax-error 'make-rash-module-begin-transformer "unexpected arguments" rest-stx)])
       (with-syntax ([mk-input (opref tab '#:in #'(current-input-port))]
                     [mk-output (opref tab '#:out #'(current-output-port))]
                     [mk-err-output (opref tab '#:err #'(current-error-port))]
                     [mk-default-starter (opref tab '#:default-starter
                                             #'#'=quoting-basic-unix-pipe=)]
                     [mk-default-line-macro (opref tab '#:default-line-macro
                                                #'#'pipeline-line-macro)])
         #'(syntax-parser
             [(_ arg (... ...))
              #'(#%plain-module-begin
                 (module* configure-runtime #f
                   (current-read-interaction
                    (λ (src in)
                      ;; TODO - this is totally broken
                      ;; TODO - This really should be as close as using repl.rkt as possible
                      (let ([stx (linea-read-syntax src in)])
                        (if (eof-object? stx)
                            stx
                            (syntax-parse stx
                              [e #'(rash-expressions-begin
                                    ((current-input-port)
                                     (current-output-port)
                                     (current-error-port)
                                     default-pipeline-starter
                                     default-line-macro)
                                    e)]))))))
                 (rash-expressions-begin (mk-input
                                          mk-output
                                          mk-err-output
                                          mk-default-starter
                                          mk-default-line-macro)
                                         arg (... ...)))]))]))

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
                'make-rash-module-begin-transformer
                "unexpected arguments"
                rest-stx)])
       (with-syntax ([mk-input (opref tab '#:in #'(open-input-string ""))]
                     [mk-output (opref tab '#:out #'default-output-port-transformer)]
                     [mk-err-output (opref tab '#:err #''string-port)]
                     ;; TODO - make it possible for these to inherit
                     [mk-default-starter (opref tab '#:default-starter
                                                #'#'=quoting-basic-unix-pipe=)]
                     [mk-line-macro (opref tab '#:default-line-macro
                                                   #'#'pipeline-line-macro)])
         #'(λ (stx)
             (syntax-parse stx
               [(rash tx-arg (... ...))
                (define-values (tab rest-stx)
                  (parse-keyword-options #'(tx-arg (... ...))
                                         rash-keyword-table
                                         #:context stx
                                         #:no-duplicates? #t))

                (with-syntax ([(parsed-rash-code (... ...))
                               (linea-stx-strs->stx rest-stx)]
                              [input (opref tab '#:in #'mk-input)]
                              [output (opref tab '#:out #'mk-output)]
                              [err-output (opref tab '#:err #'mk-err-output)]
                              [default-starter (opref tab '#:default-starter
                                                      #'mk-default-starter)]
                              [line-macro (opref tab '#:default-line-macro
                                                 #'mk-line-macro)])
                  #'(rash-expressions-begin (input output err-output
                                                   default-starter
                                                   line-macro
                                                   ;#'pipeline-line-macro
                                                   )
                                            parsed-rash-code (... ...)))])))]))
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
          (with-syntax ([(parsed-rash-code ...) (rash-stx-strs->stx #'(arg ...))])
            #'(rash-expressions-begin
               ((open-input-string "")
                default-output-port-transformer
                'string-port)
               parsed-rash-code ...))])))))

