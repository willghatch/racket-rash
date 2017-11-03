#lang racket/base

(provide define-simple-pipeline-alias)
(require
 "good-unix-operator.rkt"
 (for-syntax
  racket/base
  syntax/parse
  "rash-alias.rkt"
  ))

(define-syntax (define-simple-pipeline-alias stx)
  (syntax-parse stx
    [(_ name template-arg ...)
     #'(define-syntax name
         (rash-alias
          (syntax-parser
            [(_ arg (... ...))
             #'(=default-unix-pipe= template-arg ... arg (... ...))])))]))
