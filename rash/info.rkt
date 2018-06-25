#lang info

(define racket-launcher-names '("rash-repl"))
(define racket-launcher-libraries '("repl.rkt"))

(define deps '(("base" #:version "6.12")
               "basedir"
               "shell-pipeline"
               "linea"
               "udelim"
               "scribble-lib"
               "scribble-doc"
               "racket-doc"
               "rackunit-lib"
               "readline-lib"
               ;; for the make demo
               "make"
               ;; temporarily
               "text-table"
               ))

(define version "0.2")

