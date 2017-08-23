#lang racket/base

(provide
 define-rash-alias
 define-simple-rash-alias
 )

(require
 "../pipeline-macro.rkt"
 (for-syntax
  racket/base
  syntax/parse
  "rash-alias.rkt"
  ))

(define-syntax (define-rash-alias stx)
  (syntax-parse stx
    [(_ name transformer)
     #'(define-syntax name (rash-alias transformer))]))

(define-syntax (define-simple-rash-alias stx)
  (syntax-parse stx
    [(_ name template-arg ...)
     #'(define-syntax name
         (rash-alias
          (syntax-parser
            [(_ arg (... ...))
             #'(=quoting-basic-unix-pipe= template-arg ... arg (... ...))])))]))
