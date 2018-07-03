#lang racket/base
(require "private/subprocess-pipeline.rkt")
(define run-pipeline run-subprocess-pipeline)
(provide
 (all-from-out "private/subprocess-pipeline.rkt")
 run-pipeline
 )
