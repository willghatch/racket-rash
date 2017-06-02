#lang racket/base

;; struct properties and syntax classes for defining and detecting pipeline operators

(provide
 (all-defined-out))

(require syntax/parse)


(define-values (prop:rash-pipeline-starter
                rash-pipeline-starter?
                rash-pipeline-starter-ref)
  (make-struct-type-property 'rash-pipeline-starter))
(define-values (prop:rash-pipeline-joiner
                rash-pipeline-joiner?
                rash-pipeline-joiner-ref)
  (make-struct-type-property 'rash-pipeline-joiner))

(struct rash-pipeline-operator
  (as-starter as-joiner outside-rash-macro)
  #:property prop:rash-pipeline-starter (λ (inst . args)
                                          (apply
                                           {rash-pipeline-operator-as-starter inst}
                                           args))
  #:property prop:rash-pipeline-joiner (λ (inst . args)
                                         (apply
                                          {rash-pipeline-operator-as-joiner inst}
                                          args))
  #:property prop:procedure (struct-field-index outside-rash-macro))


(define-syntax-class pipe-starter-op
  (pattern op:id
           #:when (rash-pipeline-starter? (syntax-local-value #'op (λ () #f)))))
(define-syntax-class pipe-joiner-op
  (pattern op:id
           #:when (rash-pipeline-joiner? (syntax-local-value #'op (λ () #f)))))
(define-syntax-class not-pipeline-op
  (pattern (~and (~not x:pipe-joiner-op)
                 (~not x:pipe-starter-op))))

