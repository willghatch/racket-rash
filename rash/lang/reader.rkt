#lang s-exp syntax/module-reader
rash/lang/module-begin
#:read-syntax rash-read-syntax
#:read rash-read
;;#:whole-body-readers? #t

(require "../private/read-funcs.rkt")
