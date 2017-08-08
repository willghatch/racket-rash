#lang racket/base

(provide
 ;rash-template-escape
 )

(require
 (for-syntax
  syntax/parse
  syntax/keyword
  udelim
  syntax/strip-context
  "read-funcs.rkt"
  "misc-utils.rkt"
  (for-syntax
   syntax/parse
   syntax/keyword
   udelim
   syntax/strip-context
   "read-funcs.rkt"
   "misc-utils.rkt"
   ))

 racket/base
 "pipeline-operator-implicit.rkt"
 "parse-funcs.rkt"
 racket/stxparam
 )

#;(begin-for-syntax
  (define-syntax rash-template-escape
    (template-escape-struct
     (λ (stx)
       (syntax-parse stx
         [(_ orig-arg ...)
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
            #'(rash-expression-begin (input output err-output) parsed-rash-code)
            )])))))

