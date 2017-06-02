#lang racket/base

(provide (all-defined-out))
(provide (for-syntax (all-defined-out)))

(module+ for-public
  (provide default-pipe-starter!))

(require
 racket/stxparam
 "pipeline-operators.rkt"
 (for-syntax
  racket/base
  syntax/parse
  "pipeline-operator-detect.rkt"))

(begin-for-syntax
  (struct erroring-implicit-pipe-starter ()
    #:property
    prop:rash-pipeline-starter
    (λ (stx) (raise-syntax-error
              'erroring-implicit-pipe-starter
              "You must have left a pipeline starter implicit in a context with no default."
              stx))))

(define-syntax default-pipe-starter (erroring-implicit-pipe-starter))

(define-for-syntax implicit-pipe-starter-hash
    (make-hash (list (cons 'top-level #'default-pipe-starter))))
;; TODO - use a box!
(define-syntax-parameter implicit-pipe-starter-key
  'top-level)

;; TODO - if the macro to change the starter is in a stop-list of local-expand,
;; it can cause the default to be wrong because it works via side-effect!
;; How can this be fixed?
;; Maybe instead of letting it run as a macro, the outer rash macro can detect
;; it and eagerly do all side-effect-y things.  This would also require eagerly
;; splitting pipelines.
(define-syntax (default-pipe-starter! stx)
  (syntax-parse stx
    [(_ new-starter:pipe-starter-op)
     (begin
       (hash-set! implicit-pipe-starter-hash
                  {syntax-parameter-value #'implicit-pipe-starter-key}
                  #'new-starter)
       #'(void))]))
