#lang racket/base

(provide
 linea-line-parse
 )

(require
 "line-macro.rkt"
 (for-syntax
  racket/base
  syntax/parse
  ))

(define-syntax (linea-line-parse stx)
  (syntax-parse stx
    [(rec arg ...)
     (syntax-parse #'(arg ...)
       #:datum-literals (%%linea-racket-line %%linea-line-start)
       [((%%linea-line-start arg ...) post ...+)
        #'(begin (do-line-macro arg ...)
                 (rec post ...))]
       [((%%linea-line-start arg ...))
        #'(do-line-macro arg ...)]
       [((%%linea-racket-line arg ...) post ...+)
        #'(begin arg ...
                 (rec post ...))]
       [((%%linea-racket-line arg ...))
        #'(begin arg ...)]
       [() #'(void)])]))

