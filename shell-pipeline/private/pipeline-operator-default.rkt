#lang racket/base

(provide
 default-pipeline-starter
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
      'erroring-default-pipeline-starter
      "No explicit pipeline starter given in a context with no default set."
      #'e1)]))

(define-syntax default-pipeline-starter-macro
  (rash-pipeline-operator default-pipeline-error
                          default-pipeline-error
                          default-pipeline-error))

(define-syntax-parameter default-pipeline-starter #'default-pipeline-starter-macro)
