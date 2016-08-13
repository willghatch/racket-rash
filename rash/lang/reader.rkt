#lang s-exp syntax/module-reader
rash/lang/module-begin
#:read-syntax rash-read-syntax
#:read rash-read
#:whole-body-readers? #t

(require "../read-funcs.rkt")
(require (only-in scribble/reader
                  read-syntax-inside
                  read-inside))
