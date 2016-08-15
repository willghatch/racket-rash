#lang racket/base

(provide rash-read-syntax
         rash-read
         parse-at-reader-output
         )
(require "readtable.rkt")
(require (prefix-in scribble: scribble/reader))
(require racket/function)

(define (parse-at-reader-output argl
                                #:src [src #f])
  (for/fold ([out-list '()])
            ([str-or-atout (syntax->list argl)])
    (if (string? (syntax->datum str-or-atout))
        (append
         out-list
         (rash-read-syntax-seq src (open-input-string (syntax->datum str-or-atout))))
        (append out-list (list str-or-atout)))))

(define (rash-read-syntax-seq src in)
  (let ([result (parameterize ([current-readtable line-readtable])
                  (read-syntax src in))])
    (if (equal? eof result)
        '()
        (cons result (rash-read-syntax-seq src in)))))

(define (rash-read-syntax src in)
  (let ([at-output (scribble:read-syntax-inside src in)])
    (parse-at-reader-output at-output #:src src)))

(define (rash-read in)
  (syntax->datum (rash-read-syntax #f in)))
