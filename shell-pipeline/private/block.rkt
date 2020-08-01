#lang racket
(provide
 rash-block/splice
 (for-syntax
  lift-bind!))

(require
  (for-syntax
   racket/list
   ee-lib
   syntax/parse))

(begin-for-syntax
  (define lift-bind! (make-parameter #f)))

(define-syntax rash-block/splice
  (non-module-begin-macro
    (syntax-parser
      [(_ body-e)
       (define defs '())

       (define (this-lift-bind! binder e)
         (define bound (bind! binder (racket-var)))
         (set! defs (cons #`(#,(syntax-local-introduce-splice bound) #,e) defs))
         bound)

       (with-scope sc
         (parameterize ([lift-bind! this-lift-bind!])
           (define body^
             (local-expand #'body-e
                           (list (current-ctx-id))
                           (list #'begin)
                           (current-def-ctx)))
           (define defs^ (map (lambda (stx) #`(define . #,(syntax-local-introduce stx))) (reverse defs)))

           (splice-from-scope
            (if (eq? 'expression (syntax-local-context))
                #`(let () #,@defs^ #,body^)
                #`(begin #,@defs^ #,body^))
            sc)))])))

