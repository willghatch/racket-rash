#lang racket/base
(require "private/subprocess-pipeline.rkt")
(define run-pipeline run-subprocess-pipeline)
(define run-pipeline/out run-subprocess-pipeline/out)
(provide
 (all-from-out "private/subprocess-pipeline.rkt")
 run-pipeline
 run-pipeline/out
 )
