#lang racket/base

(provide (all-defined-out))
(provide (for-syntax (all-defined-out)))

(module+ for-public
  (provide default-pipeline-starter!))

(require
 racket/stxparam
 "pipeline-operators.rkt"
 (for-syntax
  racket/base
  syntax/parse
  "pipeline-operator-detect.rkt"))

(define-for-syntax (default-pipeline-error stx)
  (raise-syntax-error
   'erroring-default-pipeline-starter
   "No explicit pipeline starter given in a context with no default set."
   stx))

(define-syntax default-pipeline-starter
  (rash-pipeline-operator default-pipeline-error
                          default-pipeline-error
                          default-pipeline-error))

(define-for-syntax default-pipeline-starter-hash
  ;; TODO - what should the default really be?  I don't want to inherit from the top level in non-rash modules
  (make-hash (list (cons 'top-level #'default-pipeline-starter))))
;; TODO - use a box!
(define-syntax-parameter default-pipeline-starter-key
  'top-level)

;; TODO - if the macro to change the starter is in a stop-list of local-expand,
;; it can cause the default to be wrong because it works via side-effect!
;; How can this be fixed?
;; Maybe instead of letting it run as a macro, the outer rash macro can detect
;; it and eagerly do all side-effect-y things.  This would also require eagerly
;; splitting pipelines.
(define-syntax (default-pipeline-starter! stx)
  (syntax-parse stx
    [(_ new-starter:pipe-starter-op)
     (begin
       (hash-set! default-pipeline-starter-hash
                  {syntax-parameter-value #'default-pipeline-starter-key}
                  #'new-starter)
       #'(void))]))
