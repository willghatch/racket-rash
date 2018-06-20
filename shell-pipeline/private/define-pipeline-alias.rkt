#lang racket/base

(provide define-pipeline-alias)

(require
 (for-syntax
  racket/base
  syntax/parse
  "pipeline-alias.rkt"
  ))

(define-syntax (define-pipeline-alias stx)
  (syntax-parse stx
    [(_ name transformer)
     #'(define-syntax name (pipeline-alias transformer))]))

