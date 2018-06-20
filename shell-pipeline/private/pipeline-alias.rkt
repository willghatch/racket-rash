#lang racket/base

;; struct properties and syntax classes for defining and detecting aliases

(provide
 prop:pipeline-alias
 pipeline-alias?
 pipeline-alias-ref
 (rename-out [pipeline-alias-struct pipeline-alias])
 pipeline-alias-id
 )

(require syntax/parse)


(define-values (prop:pipeline-alias
                pipeline-alias?
                pipeline-alias-ref)
  (make-struct-type-property 'pipeline-alias))

(struct pipeline-alias-struct
  (transformer)
  #:property prop:pipeline-alias (λ (inst . args)
                                   (apply
                                    {pipeline-alias-struct-transformer inst}
                                    args))
  #:property prop:procedure (struct-field-index transformer))


(define-syntax-class pipeline-alias-id
  (pattern op:id
           #:when (pipeline-alias? (syntax-local-value #'op (λ () #f)))))
