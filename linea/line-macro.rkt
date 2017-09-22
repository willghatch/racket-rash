#lang racket/base

;; struct properties and syntax classes for defining and detecting line macros

(provide
 do-line-macro
 default-line-macro
 define-line-macro
 )

(require
 racket/stxparam
 (for-syntax
  "private/line-macro-detect.rkt"
  racket/base
  syntax/parse
  ))


(define-syntax (define-line-macro stx)
  (syntax-case stx ()
    [(_ name transformer)
     #'(define-syntax name (line-macro-struct transformer))]))

(define-syntax (do-line-macro stx)
  ;; detect line macros and apply them, or transform into pipeline
  (syntax-parse stx
    [(_ arg1:line-macro arg ...)
     (linea-line-macro-transform #'(arg1 arg ...))]
    [(_ arg ...)
     (let ([default-macro (syntax-parameter-value #'default-line-macro)])
       (linea-line-macro-transform #`(#,default-macro arg ...)))]))

(define-line-macro erroring-default-line-macro
  (λ (stx)
    (raise-syntax-error 'erroring-default-line-macro
                        "Currently the default line macro is just an error."
                        stx)))

(define-syntax-parameter default-line-macro #'erroring-default-line-macro)
