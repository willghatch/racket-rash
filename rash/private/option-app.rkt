#lang racket/base

;; This is generally useful -- it could be in a separate library.

(provide option-app)
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(begin-for-syntax
  (define-splicing-syntax-class kw-pair
    (pattern (~seq key:keyword arg:expr))))

(define-syntax (option-app stx)
  ;; usage: (option-app function #:key keyarg #:key2 keyarg2 positional-arg1 pos-arg2 ...)
  (syntax-parse stx
    [(__ f:expr kwarg:kw-pair ... parg:expr ...)
     (let* ([kwarg-pairs (syntax->list #'((kwarg.key . kwarg.arg) ...))]
            ;; keyword-apply needs sorted lists of keywords.  Let's do it statically.
            [sorted-pairs (sort kwarg-pairs
                                keyword<?
                                #:key (λ (x) (car (syntax->datum x)))
                                #:cache-keys? #t)]
            [keys (map (λ (x) (car (syntax-e x)))
                       sorted-pairs)]
            [args (map (λ (x) (cdr (syntax-e x)))
                       sorted-pairs)])
       (with-syntax* ([(key ...) (datum->syntax stx keys)]
                      [(arg ...) (datum->syntax stx args)]
                      [pairs #'(list `(key . ,arg) ...)])
         #'(optional-keyword-apply f pairs
                                   (list parg ...))))]))

(define (optional-keyword-apply f kwarg-pairs other-args)
  (let-values ([(kw-req kw-opt) (procedure-keywords f)])
    (let* ([all-kw-accept (if kw-opt
                              (append kw-req kw-opt)
                              #f)]
           [avail-kwarg-pairs (if all-kw-accept
                                  (filter (λ (p) (member (car p) all-kw-accept))
                                          kwarg-pairs)
                                  kwarg-pairs)]
           [avail-kws (map car avail-kwarg-pairs)]
           [avail-kwargs (map cdr avail-kwarg-pairs)])
      (keyword-apply f avail-kws avail-kwargs other-args))))


(module+ test
  (require rackunit)

  (define (f1 #:a [a 0])
    (+ a))
  (define (f2 #:a [a 0] #:c [c 0])
    (+ a c))
  (define (f3 #:a [a 0] #:b [b 0] #:c [c 0])
    (+ a b c))
  (define (f4 #:a [a 0] #:b [b 0] #:c [c 0] d e)
    (+ a b c d e))

  (check-equal? (option-app f1 #:a 2 #:c 5 #:z 7) 2)
  (check-equal? (option-app f2 #:a 2 #:c 5 #:z 7) 7)
  (check-equal? (option-app f3 #:a 2 #:c 5 #:z 7) 7)
  (check-equal? (option-app f4 #:a 2 #:c 5 #:b 7 1 3) 18)
  ;; positional arguments are still required to be correct
  (check-exn exn? (λ () (option-app f1 #:a 2 3 4 5)))
  (check-exn exn? (λ () (option-app f4 #:a 2)))
  )
