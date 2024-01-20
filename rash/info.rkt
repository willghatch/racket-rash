#lang info

(define racket-launcher-names '("rash-repl"))
(define racket-launcher-libraries '("repl.rkt"))

(define deps '(("base" #:version "8.3.0.11")
               "basedir"
               "shell-pipeline"
               "linea"
               "udelim"
               "scribble-lib"
               "scribble-doc"
               "racket-doc"
               "rackunit-lib"
               "expeditor-lib"
               ))

(define test-omit-paths '("demo/use-make-dumb-example.rkt"))

(define version "0.3")
(define license '(Apache-2.0 OR MIT))
