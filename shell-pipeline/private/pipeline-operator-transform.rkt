#lang racket/base

(provide (all-defined-out))

(require (for-syntax
          racket/base
          syntax/parse
          "pipeline-operator-detect.rkt"
          ))

(define-syntax (transform-starter-segment stx)
  (syntax-parse stx
    [(tr op:pipeline-starter arg:not-pipeline-op ...)
     (let* ([transformed (pipeline-starter-transform #'(op arg ...))])
       (syntax-parse transformed
         ;; If the transformed result is another pipeline operator, try again
         [(op:pipeline-starter arg:not-pipeline-op ...)
          #'(tr op arg ...)]
         [_ transformed]))]))

(define-syntax (transform-joint-segment stx)
  (syntax-parse stx
    [(tr op:pipeline-joint arg:not-pipeline-op ...)
     (let* ([transformed (pipeline-joint-transform #'(op arg ...))])
       (syntax-parse transformed
         ;; If the transformed result is another pipeline operator, try again
         [(op:pipeline-joint arg:not-pipeline-op ...)
          #'(tr op arg ...)]
         [_ transformed]))]))
