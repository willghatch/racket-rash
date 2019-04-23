#lang racket/base

(provide
 #%shell-pipeline/default-pipeline-starter
 )

(require
 "pipeline-operators.rkt"
 racket/stxparam
 (for-syntax
  racket/base
  syntax/parse
  ))

(define-for-syntax (default-pipeline-error stx)
  (syntax-parse stx
    [(_ e1 e2 ...)
     (raise-syntax-error
      '#%shell-pipeline/default-pipeline-starter
      "No explicit pipeline starter given in a context with no default set."
      #'e1)]))

(define-pipeline-operator #%shell-pipeline/default-pipeline-starter
  #:operator default-pipeline-error)

