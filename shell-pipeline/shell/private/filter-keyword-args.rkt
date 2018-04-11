#lang racket/base

(provide filter-keyword-args)

(require syntax/parse)

(define (filter-keyword-args stx)
  (syntax-parse stx
    [() (values '() '())]
    [(kw:keyword a:expr e ...)
     (let-values ([(kwargs pargs) (filter-keyword-args #'(e ...))])
       (values (list* #'kw #'a kwargs) pargs))]
    [(e1:expr e ...)
     (let-values ([(kwargs pargs) (filter-keyword-args #'(e ...))])
       (values kwargs (cons #'e1 pargs)))]))
