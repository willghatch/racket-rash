#lang racket/base

(provide
 default-pipeline-starter
 )

(require
 "pipeline-operators.rkt"
 racket/stxparam
 (for-syntax
  racket/base
  "pipeline-operator-detect.rkt"))

(define-for-syntax (default-pipeline-error stx)
  (raise-syntax-error
   'erroring-default-pipeline-starter
   "No explicit pipeline starter given in a context with no default set."
   stx))

(define-syntax default-pipeline-starter-macro
  (rash-pipeline-operator default-pipeline-error
                          default-pipeline-error
                          default-pipeline-error))

(define-syntax-parameter default-pipeline-starter #'default-pipeline-starter-macro)
