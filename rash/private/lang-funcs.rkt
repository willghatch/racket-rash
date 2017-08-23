#lang racket/base

(provide
 rash
 rash/wired

 ;; TODO - what should be provided from the pipeline libraries?
 (all-from-out shell/mixed-pipeline)

 (all-from-out shell/pipeline-macro)
 (all-from-out "line-macros.rkt")
 (all-from-out (submod "line-parse.rkt" for-public))

 )

(module+ experimental
  (provide (for-syntax rash-template-escaper)))

(module+ for-module-begin
  (provide rash-module-begin))

(module+ for-repl
  (provide
   rash-set-defaults
   rash-line-parse
   rash-read-and-line-parse
   ))


(require
 shell/mixed-pipeline
 racket/stxparam
 racket/splicing
 racket/string
 racket/port
 "read-funcs.rkt"
 "line-parse.rkt"
 shell/pipeline-macro
 (submod "line-parse.rkt" for-public)
 "line-macros.rkt"
 (only-in shell/private/pipeline-macro-parse rash-set-defaults)

 (for-syntax
  syntax/keyword
  racket/dict
  racket/base
  racket/stxparam-exptime
  syntax/parse
  syntax/strip-context
  udelim
  "read-funcs.rkt"
  shell/private/misc-utils

  (for-syntax
   racket/base
   syntax/parse
   "read-funcs.rkt"
   "template-escape-detect.rkt"
   )))

(define default-output-port-transformer (λ (p) (string-trim (port->string p))))

(define-for-syntax rash-keyword-table
  (list (list '#:in check-expression)
        (list '#:out check-expression)
        (list '#:err check-expression)))

(define-syntax (rash-expressions-begin stx)
  (syntax-parse stx
    [(_ (input output err-output) e ...+)
     #`(splicing-let ([in-eval input]
                      [out-eval output]
                      [err-eval err-output])
         (splicing-with-default-pipeline-starter #,(get-default-pipeline-starter)
           (rash-set-defaults
            (in-eval out-eval err-eval)
            (rash-line-parse e ...))))]))

(define-syntax (rash-module-begin stx)
  (syntax-parse stx
    [(_ arg ...)
     #'(#%plain-module-begin
        (module* configure-runtime #f
          (current-read-interaction rash-read-and-line-parse))
        (rash-expressions-begin ((current-input-port)
                                 (current-output-port)
                                 (current-error-port))
                                arg ...))]))


(define-syntax (rash stx)
  (syntax-parse stx
    [(rash orig-arg ...)
     (define-values (tab rest-stx)
       (parse-keyword-options #'(orig-arg ...)
                              rash-keyword-table
                              #:context stx
                              #:no-duplicates? #t))

     (with-syntax ([(parsed-rash-code ...)
                    (rash-stx-strs->stx rest-stx)]
                   [input (opref tab '#:in #'(open-input-string ""))]
                   [output (opref tab '#:out #'default-output-port-transformer)]
                   [err-output (opref tab '#:err #''string-port)])
       #'(rash-expressions-begin (input output err-output) parsed-rash-code ...))]))


(define-syntax (rash/wired stx)
  (syntax-parse stx
    [(_ arg ...)
     (define-values (tab rest-stx)
       (parse-keyword-options #'(arg ...)
                              rash-keyword-table
                              #:context stx
                              #:no-duplicates? #t))
     (with-syntax ([(code-segs-hopefully ...) rest-stx]
                   [in (opref tab '#:in #'(current-input-port))]
                   [out (opref tab '#:out #'(current-output-port))]
                   [err (opref tab '#:err #'(current-error-port))])
       #'(rash #:in in #:out out #:err err code-segs-hopefully ...))]))

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
