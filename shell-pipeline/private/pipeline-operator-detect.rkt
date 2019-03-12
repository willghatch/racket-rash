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

(require
 syntax/parse
 (for-syntax
  racket/base
  syntax/parse
  version/utils
  ))

;; I need to do the scope dance (double syntax-local-introduce + custom mark)
;; to make it properly hygienic, but I want to use the new 7.0 API to do it.
;; But I also don't want to bump the version required yet.
;; Since in practice these hygiene conflicts don't happen much for
;; pipeline operators, I'll just let users of old versions have potential
;; hygiene bugs.
(define-syntax (if7 stx)
  (syntax-parse stx
    [(_ then else)
     (if (version<=? "7.0" (version))
         #'then
         #'else)]))

(if7 (require syntax/apply-transformer) (void))

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

(define (pipeline-op-transform op op-form prop-ref)
  (let* ([slv (syntax-local-value op (λ () #f))]
         [prop-val (prop-ref slv)]
         [transformer (cond [(procedure? prop-val)
                             (λ (form) (prop-val slv form))]
                            [(number? prop-val)
                             {vector-ref (struct->vector slv)
                                         (add1 prop-val)}]
                            [else (raise-syntax-error
                                   'pipeline-op-transform
                                   "bad transformer value"
                                   op)])])
    (if7
     (local-apply-transformer transformer op-form 'expression)
     (transformer op-form))))

(define (pipeline-starter-transform op-form)
  (syntax-parse op-form
    [(op:pipeline-starter arg:not-pipeline-op ...)
     (pipeline-op-transform #'op op-form pipeline-starter-ref)]))
(define (pipeline-joint-transform op-form)
  (syntax-parse op-form
    [(op:pipeline-joint arg:not-pipeline-op ...)
     (pipeline-op-transform #'op op-form pipeline-joint-ref)]))


(struct pipeline-operator
  (as-starter as-joint outside-pipeline-macro)
  #:transparent
  #:property prop:pipeline-starter (struct-field-index as-starter)
  #:property prop:pipeline-joint (struct-field-index as-joint)
  #:property prop:procedure (struct-field-index outside-pipeline-macro))
