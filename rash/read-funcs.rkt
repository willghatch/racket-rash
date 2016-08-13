#lang racket/base

(provide rash-read-syntax
         rash-read
         parse-at-reader-output
         )
(require "readtable.rkt")
(require (prefix-in scribble: scribble/reader))
(require racket/function)

(define (rash-read-syntax* src in)
  (parameterize ([current-readtable line-readtable])
    (read-syntax src in)))

(define (parse-at-reader-output argl
                                #:src [src #f])
  (let* ([stx? (syntax? argl)]
         [->datum (if stx? syntax->datum identity)]
         [finalize (if stx? identity syntax->datum)]
         [->list (if stx? syntax->list identity)])
    (finalize
     (for/fold ([out-list '()])
               ([str-or-atout (->list argl)])
       (if (string? (->datum str-or-atout))
           (append
            out-list
            (rash-read-syntax-seq src (open-input-string (->datum str-or-atout))))
           (append out-list (list str-or-atout)))))))

(define (rash-read-syntax-seq src in)
  (let ([result (rash-read-syntax* src in)])
    (if (equal? eof result)
        '()
        (cons result (rash-read-syntax-seq src in)))))

(define (rash-read-syntax src in)
  (let ([at-output (scribble:read-syntax-inside src in)])
    (parse-at-reader-output at-output #:src src)))

(define (rash-read in)
  (syntax->datum (rash-read-syntax #f in)))
