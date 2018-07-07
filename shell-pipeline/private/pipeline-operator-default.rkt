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
  "pipeline-operator-detect.rkt"))

(define-for-syntax (default-pipeline-error stx)
  (syntax-parse stx
    [(_ e1 e2 ...)
     (raise-syntax-error
      '#%shell-pipeline/default-pipeline-starter
      "No explicit pipeline starter given in a context with no default set."
      #'e1)]))

(define-syntax #%shell-pipeline/default-pipeline-starter
  (pipeline-operator default-pipeline-error
                     default-pipeline-error
                     default-pipeline-error))

