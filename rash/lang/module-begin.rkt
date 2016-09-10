#lang racket/base

(require rash)
(provide
 (except-out (all-from-out racket/base) #%module-begin)
 (rename-out [module-begin #%module-begin])
 (all-from-out rash)
 )

(require (for-syntax racket/base
                     syntax/parse))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(m-b arg ...)
     #'(#%plain-module-begin (rash-line-parse arg ...))]))
