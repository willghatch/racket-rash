#lang racket/base

;; struct properties and syntax classes for defining and detecting line macros

(provide
 prop:line-macro
 line-macro?
 linea-line-macro-transform
 line-macro
 line-macro-struct
 )
(module+ for-public
  (provide
   prop:line-macro
   line-macro?
   line-macro
   ))

(require
 syntax/parse
 (for-syntax
  racket/base
  syntax/parse
  version/utils
  ))

;; Like pipeline-operator-detect.rkt, I need to do the scope dance for hygiene, but
;; I don't want to require a new version of Racket yet.
(define-syntax (if7 stx)
  (syntax-parse stx
    [(_ then else)
     (if (version<=? "7.0" (version))
         #'then
         #'else)]))
(if7 (require syntax/apply-transformer) (void))

(define-values (prop:line-macro
                line-macro?
                linea-line-macro-ref)
  (make-struct-type-property 'linea-line-macro))

(define-syntax-class line-macro
  (pattern op:id
           #:when (line-macro? (syntax-local-value #'op (λ () #f)))))

(define (linea-line-macro-transform stx)
  (syntax-parse stx
    [(lm:line-macro arg ...)
     (let* ([slv (syntax-local-value #'lm)]
            [prop-val (linea-line-macro-ref slv)]
            [transformer (cond [(procedure? prop-val)
                                (λ (form) (prop-val slv form))]
                               [(number? prop-val)
                                (λ (form) ({vector-ref (struct->vector slv)
                                                       (add1 prop-val)}
                                           form))])])
       (if7
        (local-apply-transformer transformer stx 'expression)
        (transformer stx)))]))

(struct line-macro-struct
  (transformer)
  #:property prop:line-macro (λ (inst . args)
                               (apply
                                {line-macro-struct-transformer inst}
                                args))
  #:property prop:procedure (struct-field-index transformer))


