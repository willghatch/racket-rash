#lang racket
(provide
 (rename-out [block/splice rash-block/splice])
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

(define-syntax block/splice
  (non-module-begin-macro
    (syntax-parser
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
         (define bound (bind! (syntax->list binders) (racket-var)))
         (define bound-posspace (map syntax-local-introduce-splice
                                     bound))
         (set! defs (cons #`(#,bound-posspace #,e) defs))
         bound)
       (define (this-lift-syntaxes! binders e)
         (move-tail!)
         (define bound (bind! (syntax->list binders) (eval-transformer e)))
         (define bound-posspace (map syntax-local-introduce-splice
                                     bound))
         (set! stxs (cons #`(#,bound-posspace #,e) stxs))
         bound)
       (define (this-lift-expression! e)
         (when (not (syntax? e))
           (error 'lift-expression! "not syntax: ~v" e))
         (set! tail-exprs (cons (syntax-local-introduce e) tail-exprs)))


       (with-scope sc
         (parameterize ([lift-binds! this-lift-binds!]
                        [lift-syntaxes! this-lift-syntaxes!]
                        [lift-expression! this-lift-expression!]
                        [current-ctx-id (block-context-id)])
           (let loop ([todo (syntax->list (add-scope #'(body ...) sc))])
             (unless (null? todo)
               (define expanded
                 (local-expand (first todo)
                               (list (current-ctx-id))
                               (list #'begin #'define-syntaxes #'define-values)
                               (current-def-ctx)))
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
           (define final-body
             #`(#,@(map (lambda (stx)
                          (syntax-parse stx
                            [((id ...) rhs)
                             (define/syntax-parse (id^ ...) (map syntax-local-introduce-splice (syntax->list #'(id ...)))) 
                             (define/syntax-parse rhs^ (syntax-local-introduce #'rhs))
                             #'(define-syntaxes (id^ ...) rhs^)]))
                        (reverse stxs))
                #,@(map (lambda (stx)
                          (syntax-parse stx
                            [((id ...) rhs)
                             (define/syntax-parse (id^ ...) (map syntax-local-introduce-splice (syntax->list #'(id ...)))) 
                             (define/syntax-parse rhs^ (syntax-local-introduce #'rhs))
                             #'(define-values (id^ ...) rhs^)]))
                        (reverse defs))
                #,@(if (null? tail-exprs)
                       #'((void))
                       (reverse (map syntax-local-introduce tail-exprs)))))
           (splice-from-scope
            (if (eq? 'expression (syntax-local-context))
                #`(let () #,@final-body)
                #`(begin #,@final-body))
            sc)))])))

(module+ test
  (require rackunit)

  (check-equal? (block/splice (define foo 5) foo)
                5)
  (check-equal? (block/splice
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
  (check-equal? (block/splice 27 (define foo 77))
                (void))
  (check-equal? (block/splice 27
                              (define-syntax (m stx)
                                (syntax-parse stx [(_ arg) #'arg]))
                              (m (define x (m 5)))
                              (list (m (list 33 x))))
                (list (list 33 5)))

  (check-equal? (let ()
                  (block/splice 27
                                (define-syntax (m stx)
                                  (syntax-parse stx [(_ arg) #'arg]))
                                (m (define x (m 5)))
                                (list (m (list 33 x)))))
                (list (list 33 5)))

  )
