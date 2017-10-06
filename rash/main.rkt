#lang racket/base

(require "private/lang-funcs.rkt")
(provide (all-from-out "private/lang-funcs.rkt"))

;(make-rash-lang-submodule lang-bindings)
;(make-rash-reader-submodule (submod rash/main lang-bindings))
(make-rash-reader-submodule rash/private/basic-module-begin)


