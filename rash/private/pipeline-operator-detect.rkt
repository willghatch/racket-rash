#lang racket/base

;; struct properties and syntax classes for defining and detecting pipeline operators

(provide
 prop:rash-pipeline-starter
 rash-pipeline-starter?
 rash-pipeline-starter-transform

 prop:rash-pipeline-joiner
 rash-pipeline-joiner?
 rash-pipeline-joiner-transform

 pipe-starter-op
 pipe-joiner-op
 not-pipeline-op

 rash-pipeline-operator
 )

(require syntax/parse)


(define-values (prop:rash-pipeline-starter
                rash-pipeline-starter?
                rash-pipeline-starter-ref)
  (make-struct-type-property 'rash-pipeline-starter))
(define-values (prop:rash-pipeline-joiner
                rash-pipeline-joiner?
                rash-pipeline-joiner-ref)
  (make-struct-type-property 'rash-pipeline-joiner))

(define-syntax-class pipe-starter-op
  (pattern op:id
           #:when (rash-pipeline-starter? (syntax-local-value #'op (λ () #f)))))
(define-syntax-class pipe-joiner-op
  (pattern op:id
           #:when (rash-pipeline-joiner? (syntax-local-value #'op (λ () #f)))))
(define-syntax-class not-pipeline-op
  (pattern (~and (~not x:pipe-joiner-op)
                 (~not x:pipe-starter-op))))


(define (rash-pipeline-starter-transform op-form)
  (syntax-parse op-form
    [(op:pipe-starter-op arg:not-pipeline-op ...)
     (let ([slv (syntax-local-value #'op (λ () #f))])
       (let ([starter (rash-pipeline-starter-ref slv)])
             (cond [(procedure? starter) (starter slv op-form)]
                   [(number? starter) ({vector-ref (struct->vector slv)
                                                   (add1 starter)}
                                       op-form)])))]))
(define (rash-pipeline-joiner-transform op-form)
  (syntax-parse op-form
    [(op:pipe-joiner-op arg:not-pipeline-op ...)
     (let ([slv (syntax-local-value #'op (λ () #f))])
       (let ([joiner (rash-pipeline-joiner-ref slv)])
             (cond [(procedure? joiner) (joiner slv op-form)]
                   [(number? joiner) ({vector-ref (struct->vector slv)
                                                   (add1 joiner)}
                                       op-form)])))]))


(struct rash-pipeline-operator
  (as-starter as-joiner outside-rash-macro)
  #:transparent
  #:property prop:rash-pipeline-starter (struct-field-index as-starter)
  #:property prop:rash-pipeline-joiner (struct-field-index as-joiner)
  #:property prop:procedure (struct-field-index outside-rash-macro))
