#lang racket/base

(provide
 linea-read-syntax-string
 )

(require
 "read.rkt"
 racket/port
 syntax/parse
 udelim
 syntax/strip-context
 )

(define (linea-read-syntax-string stx)
  (let ([src (syntax-parse stx
               [(linea-src:str) #'linea-src]
               [(src-seg:str ...+) (scribble-strings->string #'(src-seg ...))])])
    (replace-context
     src
     (datum->syntax #f (cons '#%linea-expressions-begin
                             (port->list (λ (p) (linea-read-syntax
                                                 (syntax-source src)
                                                 p))
                                         (stx-string->port src)))))))

