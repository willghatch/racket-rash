#lang racket/base

(require "lang-funcs.rkt")
(provide (all-from-out "lang-funcs.rkt")
         (rename-out [module-begin #%module-begin]))

(require (for-syntax racket/base
                     syntax/parse))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(m-b s ...)
     #'(#%module-begin (shell-line-parse s ...))]))
