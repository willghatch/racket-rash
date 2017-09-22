#lang racket/base

(provide
 default-line-macro
 )

(require
 "line-macros.rkt"
 racket/stxparam
 (for-syntax
  racket/base
  ))

(define-line-macro erroring-default-line-macro
  (λ (stx)
    (raise-syntax-error 'erroring-default-line-macro
                        "Currently the default line macro is just an error."
                        stx)))

(define-syntax-parameter default-line-macro #'erroring-default-line-macro)
