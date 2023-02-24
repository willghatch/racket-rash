#lang info

(define collection "shell")
(define deps '("base"
               "scribble-lib"
               "racket-doc"
               "rackunit-lib"
               ))
(define scribblings '(["scribblings/shell-pipeline.scrbl" (multi-page) (library)]))
(define version "0.2")
(define license '(Apache-2.0 OR MIT))

