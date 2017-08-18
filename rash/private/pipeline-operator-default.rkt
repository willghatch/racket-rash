#lang racket/base

(provide
 set-default-pipeline-starter!
 with-default-pipeline-starter
 splicing-with-default-pipeline-starter
 (for-syntax
  get-default-pipeline-starter
  ))

(module+ for-public
  (provide
   set-default-pipeline-starter!
   ))

(require
 racket/stxparam
 racket/splicing
 "pipeline-operators.rkt"
 "settable-lexical-default.rkt"
 (for-syntax
  racket/base
  syntax/parse
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

(define-settable-lexical-default
  default-pipeline-starter
  default-pipeline-starter-macro)
