#lang racket/base

(require "../readtable.rkt")
(require "../lang.rkt")
(provide (rename-out [line-read-syntax read-syntax]
                     [line-read read]))

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
      #`(module something "lang.rkt" s ...))))

(define (line-read in)
  (syntax->datum (line-read-syntax #f in)))
