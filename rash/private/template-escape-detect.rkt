#lang racket/base

;; struct properties and syntax classes for defining and detecting template escapes

(provide
 prop:template-escape
 template-escape?
 template-escape-transform
 template-escape
 template-escape-struct
 )

(require syntax/parse)


(define-values (prop:template-escape
                template-escape?
                template-escape-ref)
  (make-struct-type-property 'template-escape))

(define-syntax-class template-escape
  (pattern op:id
           #:when (template-escape? (syntax-local-value #'op (λ () #f)))))

(define (template-escape-transform stx)
  (syntax-parse stx
    [(lm:template-escape arg ...)
     (let ([slv (syntax-local-value #'lm)])
       (let ([transform (template-escape-ref slv)])
         (cond [(procedure? transform) (transform slv stx)]
               [(number? transform) ({vector-ref (struct->vector slv)
                                                 (add1 transform)}
                                     stx)])))]))

(struct template-escape-struct
  (transformer)
  #:property prop:template-escape (λ (inst . args)
                                    (apply
                                     {template-escape-struct-transformer inst}
                                     args))
  ;#:property prop:procedure (struct-field-index transformer)
  )


