#lang racket/base

(provide rash-read-syntax
         rash-read
         )
(require "readtable.rkt")

(define (rash-read-syntax src in)
  (parameterize ([current-readtable line-readtable])
    (read-syntax src in)))
(define (rash-read in)
  (parameterize ([current-readtable line-readtable])
    (read in)))
