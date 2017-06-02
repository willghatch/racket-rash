#lang racket/base

(provide or/exn)

(require (for-syntax racket/base syntax/parse))

(define-syntax (or/exn stx)
  (syntax-parse stx
    [(_ arg1)
     #'arg1]
    [(_ arg1 arg ...+)
     #'(with-handlers ([(λ (e) #t) (λ (e) (or/exn arg ...))])
         arg1)]))

(module+ test
  (require rackunit)
  (check-equal? (or/exn 5 3) 5)
  (check-equal? (or/exn (error 'hi) 7 9) 7)
  (check-equal? (or/exn (error 'hi) (error 'bye) 7 9) 7)
  (check-exn (λ _ #t) (λ () (or/exn (error 'hi) (error 'bye))))
  )
