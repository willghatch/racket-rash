#lang racket/base

(provide (all-defined-out))

(require (for-syntax
          racket/base
          syntax/parse
          "pipeline-operator-detect.rkt"
          ))

(define-syntax (rash-transform-starter-segment stx)
  (syntax-parse stx
    [(tr op:pipe-starter-op arg:not-pipeline-op ...)
     (let* ([slv (syntax-local-value #'op)]
            [transformed ({rash-pipeline-starter-ref slv} slv #'(op arg ...))])
       (syntax-parse transformed
         ;; If the transformed result is another pipeline operator, try again
         [(op:pipe-starter-op arg:not-pipeline-op ...)
          #'(tr op arg ...)]
         [_ transformed]))]))

(define-syntax (rash-transform-joiner-segment stx)
  (syntax-parse stx
    [(tr op:pipe-joiner-op arg:not-pipeline-op ...)
     (let* ([slv (syntax-local-value #'op)]
            [transformed ({rash-pipeline-joiner-ref slv} slv #'(op arg ...))])
       (syntax-parse transformed
         ;; If the transformed result is another pipeline operator, try again
         [(op:pipe-joiner-op arg:not-pipeline-op ...)
          #'(tr op arg ...)]
         [_ transformed]))]))
