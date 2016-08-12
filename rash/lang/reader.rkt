#lang racket/base

(require "../readtable.rkt")
(require "module-begin.rkt")
(provide (rename-out [line-read-syntax read-syntax]
                     ))

(define (read-syntax-seq* src in)
  (let ([result (read-syntax src in)])
    (if (equal? eof result)
        '()
        (cons result (read-syntax-seq src in)))))
(define (read-syntax-seq src in)
  (datum->syntax #f (read-syntax-seq* src in)))

(define (line-read-syntax src in)
  (parameterize ([current-readtable line-readtable])
    (with-syntax ([(s ...) (read-syntax-seq src in)])
      #`(module something rash/lang/module-begin s ...))))
