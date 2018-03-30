#lang racket/base

(provide
 linea-stx-strs->stx
 linea-read-syntax-all
 )

(require
 "read.rkt"
 racket/port
 syntax/parse
 udelim
 syntax/strip-context
 )

(define (linea-stx-strs->stx stx)
  (let ([src (syntax-parse stx
               [(linea-src:str) #'linea-src]
               [(src-seg:str ...+) (scribble-strings->string #'(src-seg ...))])])
    (map (λ (s) (replace-context src s))
         (syntax->list
          (linea-read-syntax-all (syntax-source src)
                                 (stx-string->port src))))))
(define (linea-read-syntax-all src p)
  (datum->syntax #f (port->list (λ (p) (linea-read-syntax src p)) p)))
