#lang racket/base

(provide define-simple-pipeline-alias)
(require
 "good-unix-operator.rkt"
 (for-syntax
  racket/base
  syntax/parse
  "pipeline-alias.rkt"
  ))

(define-syntax (define-simple-pipeline-alias stx)
  (syntax-parse stx
    [(_ name template-arg ...)
     #'(define-syntax name
         (pipeline-alias
          (syntax-parser
            [(_ arg (... ...))
             #'(=unix-pipe= template-arg ... arg (... ...))])))]))
