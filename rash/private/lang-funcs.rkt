#lang racket/base

(provide
 rash
 rash/wired

 ;; TODO - what should be provided from the pipeline libraries?
 (all-from-out shell/mixed-pipeline)

 (all-from-out "pipeline-operators.rkt")
 (all-from-out "line-macros.rkt")
 (all-from-out (submod "pipeline-operator-default.rkt" for-public))
 (all-from-out (submod "parse-funcs.rkt" for-public))

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
   rash-pipeline-opt-hash
   default-output-port-transformer
   (for-syntax default-pipeline-starter-hash)
   default-pipeline-starter-key
   ))


(require
 shell/mixed-pipeline
 racket/stxparam
 racket/splicing
 "read-funcs.rkt"
 "parse-funcs.rkt"
 "pipeline-operator-default.rkt"
 (submod "pipeline-operator-default.rkt" for-public)
 (submod "parse-funcs.rkt" for-public)
 "line-macros.rkt"
 "pipeline-operators.rkt"

 (for-syntax
  syntax/keyword
  racket/dict
  racket/base
  racket/stxparam-exptime
  syntax/parse
  syntax/strip-context
  udelim
  "read-funcs.rkt"
  "misc-utils.rkt"

  (for-syntax
   racket/base
   syntax/parse
   "read-funcs.rkt"
   "template-escape-detect.rkt"
   )))



(define-for-syntax rash-keyword-table
  (list (list '#:in check-expression)
        (list '#:out check-expression)
        (list '#:err check-expression)))

(define-syntax (rash-expressions-begin stx)
  (syntax-parse stx
    [(_ (input output err-output) e ...+)
     (let* ([default-key (gensym 'rash-default-starter-key-)]
            [set (hash-set! default-pipeline-starter-hash
                            default-key
                            (hash-ref default-pipeline-starter-hash
                                      {syntax-parameter-value
                                       #'default-pipeline-starter-key}))])
       #`(splicing-let ([in-eval input]
                        [out-eval output]
                        [err-eval err-output])
           (splicing-syntax-parameterize ([default-pipeline-starter-key
                                           (quote
                                            #,(datum->syntax #'here default-key))])
             (rash-set-defaults
              (in-eval out-eval err-eval)
              (rash-line-parse e ...)))))]))

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
