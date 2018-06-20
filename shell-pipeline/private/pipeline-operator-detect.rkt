#lang racket/base

;; struct properties and syntax classes for defining and detecting pipeline operators

(provide
 prop:pipeline-starter
 pipeline-starter?
 pipeline-starter-transform

 prop:pipeline-joint
 pipeline-joint?
 pipeline-joint-transform

 pipeline-starter
 pipeline-joint
 not-pipeline-op

 pipeline-operator
 )

(require syntax/parse)


(define-values (prop:pipeline-starter
                pipeline-starter?
                pipeline-starter-ref)
  (make-struct-type-property 'pipeline-starter))
(define-values (prop:pipeline-joint
                pipeline-joint?
                pipeline-joint-ref)
  (make-struct-type-property 'pipeline-joint))

(define-syntax-class pipeline-starter
  (pattern op:id
           #:when (pipeline-starter? (syntax-local-value #'op (λ () #f)))))
(define-syntax-class pipeline-joint
  (pattern op:id
           #:when (pipeline-joint? (syntax-local-value #'op (λ () #f)))))
(define-syntax-class not-pipeline-op
  (pattern (~and (~not x:pipeline-joint)
                 (~not x:pipeline-starter))))


(define (pipeline-starter-transform op-form)
  (syntax-parse op-form
    [(op:pipeline-starter arg:not-pipeline-op ...)
     (let ([slv (syntax-local-value #'op (λ () #f))])
       (let ([starter (pipeline-starter-ref slv)])
             (cond [(procedure? starter) (starter slv op-form)]
                   [(number? starter) ({vector-ref (struct->vector slv)
                                                   (add1 starter)}
                                       op-form)])))]))
(define (pipeline-joint-transform op-form)
  (syntax-parse op-form
    [(op:pipeline-joint arg:not-pipeline-op ...)
     (let ([slv (syntax-local-value #'op (λ () #f))])
       (let ([joint (pipeline-joint-ref slv)])
             (cond [(procedure? joint) (joint slv op-form)]
                   [(number? joint) ({vector-ref (struct->vector slv)
                                                   (add1 joint)}
                                       op-form)])))]))


(struct pipeline-operator
  (as-starter as-joint outside-pipeline-macro)
  #:transparent
  #:property prop:pipeline-starter (struct-field-index as-starter)
  #:property prop:pipeline-joint (struct-field-index as-joint)
  #:property prop:procedure (struct-field-index outside-pipeline-macro))
