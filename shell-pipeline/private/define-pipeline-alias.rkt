#lang racket/base

(provide define-pipeline-alias)

(require
 (for-syntax
  racket/base
  syntax/parse
  "rash-alias.rkt"
  ))

(define-syntax (define-pipeline-alias stx)
  (syntax-parse stx
    [(_ name transformer)
     #'(define-syntax name (rash-alias transformer))]))

