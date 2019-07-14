#lang info

(define collection "shell")
(define deps '("base"
               "scribble-lib"
               "racket-doc"
               "rackunit-lib"
               "ee-lib"
               ))
(define scribblings '(["scribblings/shell-pipeline.scrbl" (multi-page) (library)]))
(define version "0.2")

