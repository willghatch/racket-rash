#lang racket/base

(require "lang-funcs.rkt")
(provide cd)
;; Somehow this didn't work when I had this provided by lang-funcs...
(define-alias cd args (cons shell-cd args))
