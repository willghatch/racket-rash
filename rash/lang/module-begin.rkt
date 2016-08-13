#lang racket/base

(require "../lang-funcs.rkt")
(provide
 (all-from-out "../lang-funcs.rkt")
 #;(except-out (all-from-out "../lang-funcs.rkt")
             #%module-begin)

 (except-out (all-from-out racket/base) #%module-begin)
 (rename-out [module-begin #%module-begin]))

(require (for-syntax racket/base
                     syntax/parse))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(m-b s ...)
     #'(#%module-begin (rash-line-parse s ...))]))
