#lang racket/base

(require "private/lang-funcs.rkt")
(provide (all-from-out "private/lang-funcs.rkt"))

(module reader syntax/module-reader
  rash/private/basic-module-begin
  #:read-syntax linea-read-syntax
  #:read linea-read
  (require "private/linea/read.rkt")
  )
