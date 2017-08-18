#lang racket/base

(provide (all-defined-out))
(provide (for-syntax (all-defined-out)))

(module+ for-public
  (provide default-pipeline-starter!))

(require
 racket/stxparam
 racket/splicing
 "pipeline-operators.rkt"
 (for-syntax
  racket/base
  syntax/parse
  "pipeline-operator-detect.rkt"))

(begin-for-syntax
  ;; define mybox, because boxes are turned into immutable boxes (rather than
  ;; 3d syntax) when turned into syntax.
  (struct mybox ([v #:mutable])))

(define-for-syntax (default-pipeline-error stx)
  (raise-syntax-error
   'erroring-default-pipeline-starter
   "No explicit pipeline starter given in a context with no default set."
   stx))

(define-syntax default-pipeline-starter
  (rash-pipeline-operator default-pipeline-error
                          default-pipeline-error
                          default-pipeline-error))

(define-for-syntax (get-default-pipeline-starter)
  (mybox-v
   (syntax->datum
    (syntax-parameter-value #'default-pipeline-starter-param))))

(define-for-syntax default-pipeline-starter-box
  (mybox #'default-pipeline-starter))

(define-syntax-parameter default-pipeline-starter-param
  ;; TODO - what should the default really be?  I don't want to inherit from the top level in non-rash modules
  (datum->syntax #'here default-pipeline-starter-box))

;; TODO - if the macro to change the starter is in a stop-list of local-expand,
;; it can cause the default to be wrong because it works via side-effect!
;; How can this be fixed?
(define-syntax (default-pipeline-starter! stx)
  (syntax-parse stx
    [(_ new-starter:pipe-starter-op)
     (begin
       (set-mybox-v!
        (syntax->datum (syntax-parameter-value #'default-pipeline-starter-param))
        #'new-starter)
       #'(void))]))

(define-syntax (default-pipeline-starter-bound stx)
  (syntax-parse stx
    [(_ starting-default:id body ...+)
     #`(splicing-syntax-parameterize
           ([default-pipeline-starter-param
              #,(datum->syntax #'starting-default (mybox #'starting-default))])
         body ...)]))
