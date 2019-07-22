#lang racket
(provide
 (rename-out [block rash-block])
 (for-syntax
  lift-binds!
  lift-syntaxes!
  lift-expression!
  (rename-out [block-context-id? rash-block-context-id?])
  ))

(require
  (for-syntax
   racket/list
   ee-lib
   syntax/parse))

(begin-for-syntax
  (define lift-binds! (make-parameter #f))
  (define lift-syntaxes! (make-parameter #f))
  (define lift-expression! (make-parameter #f))
  (struct block-context-id ())
  )

(define-syntax (block stx)
  (syntax-parse stx
    [(_ body ...)
     (define defs '())
     (define stxs '())
     (define tail-exprs '())

     (define (move-tail!)
       (set! defs (append (map (λ (te) #`(() (begin #,te (values))))
                               tail-exprs)
                          defs))
       (set! tail-exprs '()))
     (define (this-lift-binds! binders e)
       (move-tail!)
       (define binders^ (map syntax-local-introduce-splice
                             (bind! (syntax->list binders) #f)))
       (set! defs (cons #`(#,binders^ #,e) defs))
       binders^)
     (define (this-lift-syntaxes! binders e)
       (move-tail!)
       (define binders^ (map syntax-local-introduce-splice
                             (bind! (syntax->list binders) e)))
       (set! stxs (cons #`(#,binders^ #,e) stxs))
       binders^)
     (define (this-lift-expression! e)
       (when (not (syntax? e))
         (error 'lift-expression! "not syntax: ~v" e))
       (set! tail-exprs (cons (syntax-local-introduce e) tail-exprs)))

     (ee-lib-boundary
      (parameterize ([lift-binds! this-lift-binds!]
                     [lift-syntaxes! this-lift-syntaxes!]
                     [lift-expression! this-lift-expression!]
                     [current-ctx-id (block-context-id)])
        (let loop ([todo (syntax->list #'(body ...))])
          (unless (null? todo)
            (define expanded
              (local-expand (first todo)
                            (list (current-ctx-id))
                            (list #'begin #'define-syntaxes #'define-values)
                            (cons (current-def-ctx) (current-local-def-ctxs))))
            (syntax-parse expanded
              #:literals (begin define-syntaxes define-values)
              [(begin e ...)
               (loop (append (syntax->list #'(e ...)) todo))]
              [(define-values (x ...) e)
               ((lift-binds!) #'(x ...) #'e)
               (loop (cdr todo))]
              [(define-syntaxes (x ...) e)
               ((lift-syntaxes!) #'(x ...) #'e)
               (loop (cdr todo))]
              [e
               ((lift-expression!) #'e)
               (loop (cdr todo))])))
        #`(letrec-syntaxes+values (#,@(reverse (map syntax-local-introduce stxs)))
                                  (#,@(reverse (map syntax-local-introduce defs)))
            #,@(if (null? tail-exprs)
                   #'((void))
                   (reverse (map syntax-local-introduce tail-exprs))))))]))


(module+ test
  (require rackunit)

  (check-equal? (block (define foo 5) foo)
                5)
  (check-equal? (block
                 77
                 (define (even? x)
                   (if (equal? 0 x)
                       #t
                       (odd? (sub1 x))))
                 (define (odd? x)
                   (if (equal? 0 x)
                       #f
                       (even? (sub1 x))))
                 (even? 5))
                #f)
  (check-equal? (block 27 (define foo 77))
                (void))
  (check-equal? (block 27
                       (define-syntax (m stx)
                         (syntax-parse stx [(_ arg) #'arg]))
                       (m (define x (m 5)))
                       (list (m (list 33 x))))
                (list (list 33 5)))

  )
