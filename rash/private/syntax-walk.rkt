#lang racket/base

(provide
 syntax-walk
 syntax-map)

(define (improper-map f il)
  (if (pair? il)
      (cons (f (car il))
            (improper-map f (cdr il)))
      (f il)))

(define (alternate l1 l2)
  (if (null? l1)
      '()
      (list* (car l1) (car l2) (alternate (cdr l1) (cdr l2)))))

(define (syntax-walk f stx)
  ;; f must be (-> stx (-> stx) stx)
  ;; IE f takes as arguments the original syntax object (to inspect it before
  ;; deciding whether to recur on it), and a thunk that will provide the
  ;; result of recurring.  The result of the thunk should be used in place of
  ;; the original syntax if the thunk is used.
  (let ([e (syntax-e stx)]
        [rec-walk (λ (s) (syntax-walk f s))])
    (define-syntax-rule (re-stx new old re-body)
      ;; Compare new and old syntax object lists and return either the original stx
      ;; (to save allocations, let people compare with eq?, ...), or rebuild a new one.
      (if (andmap eq? new old)
          stx
          (datum->syntax stx re-body stx stx)))
    (cond
      ;; syntax pair -- proper list
      ;; also, null
      [(list? e)
       (f stx (λ () (let ([new (map rec-walk e)])
                      (re-stx new e new))))]
      ;; syntax pair -- improper list
      [(pair? e)
       (f stx (λ () (let ([new (improper-map rec-walk e)])
                      (re-stx new e new))))]
      ;; immutable vector of stx objects
      [(vector? e)
       (f stx (λ () (let* ([old (vector->list e)]
                           [new (map rec-walk old)])
                      (re-stx new old (apply vector-immutable new)))))]
      ;; immutable box of syntax objects
      [(box? e)
       (f stx (λ () (let ([new (rec-walk (unbox e))])
                      (re-stx (list new) (list (unbox e)) (box-immutable new)))))]
      ;; immutable hash table of syntax object values (but keys are not syntax)
      [(hash? e)
       (f stx (λ () (let* ([keys (hash-keys e)]
                           [old (map (λ (k) (hash-ref e k)) keys)]
                           [new (map rec-walk old)])
                      (re-stx new old (apply
                                       {cond [(hash-eq? e) hasheq]
                                             [(hash-equal? e) hash]
                                             [else hasheqv]}
                                       (alternate keys new))))))]
      ;; immutable prefab struct of stx objects
      [(struct? e)
       (f stx (λ ()
                (let* ([vec (struct->vector e)]
                       [pkey (vector-ref vec 0)]
                       [elem-list (cdr (vector->list vec))]
                       [new-elem-list (map rec-walk elem-list)])
                  (re-stx new-elem-list elem-list (apply make-prefab-struct
                                                         pkey
                                                         new-elem-list)))))]
      ;; symbol
      ;; misc datum
      [else (f stx (λ () stx))])))

(define (syntax-map f stx)
  ;; Returns a syntax object like `stx` but with `f` applied to each node
  ;; in the tree, starting with the leaves.
  (syntax-walk (λ (s do-rec)
                 (f (do-rec)))
               stx))

[module+ test
  (require rackunit)

  (define s #'(testing (test test foo) 123))
  (define sw-id (λ (stx do-rec) (do-rec)))

  (check-equal? (syntax-walk sw-id s)
                s)

  (check free-identifier=?
         (syntax-walk sw-id #'hello)
         #'hello)

  (define (inc-if-num stx do-rec)
    (let ([s (do-rec)])
      (if (number? (syntax-e s))
          (datum->syntax stx (add1 (syntax-e s))
                         stx stx)
          s)))
  (check-equal? (syntax->datum (syntax-walk inc-if-num #'(+ 5 6)))
                (syntax->datum #'(+ 6 7)))

  ;; TODO - add some good tests...
  ]

