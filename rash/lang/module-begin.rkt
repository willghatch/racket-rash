#lang racket/base

(provide
 (except-out (all-from-out racket/base) #%module-begin)
 (rename-out [module-begin #%module-begin])
 (all-from-out rash)
 )

(require rash
         syntax/wrap-modbeg
         (for-syntax racket/base
                     syntax/parse))

(define last-ret (make-parameter 0))

(define-syntax (update-last-ret stx)
  (syntax-parse stx
    [(ulr expr)
     #'(last-ret expr)]))

(define-syntax wrapped-module-begin
  (make-wrapping-module-begin #'update-last-ret))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(m-b arg ...)
     #'(wrapped-module-begin
        (rash-line-parse arg ...)
        (cond
          [(and (exact-nonnegative-integer? (last-ret))
                (< (last-ret) 128))
           (exit (last-ret))]
          [(void? (last-ret)) (exit 0)]
          [else (begin
                  (eprintf "improper exit value: ~s~n" (last-ret))
                  (exit 126))]))]))
