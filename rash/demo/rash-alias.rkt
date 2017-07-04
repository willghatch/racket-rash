#lang racket/base

;; struct properties and syntax classes for defining and detecting aliases

(provide
 prop:rash-alias
 rash-alias?
 rash-alias-ref
 (rename-out [rash-alias-struct rash-alias])
 rash-alias-id
 )

(require syntax/parse)


(define-values (prop:rash-alias
                rash-alias?
                rash-alias-ref)
  (make-struct-type-property 'rash-alias))

(struct rash-alias-struct
  (transformer)
  #:property prop:rash-alias (λ (inst . args)
                               (apply
                                {rash-alias-struct-transformer inst}
                                args))
  #:property prop:procedure (struct-field-index transformer))


(define-syntax-class rash-alias-id
  (pattern op:id
           #:when (rash-alias? (syntax-local-value #'op (λ () #f)))))
