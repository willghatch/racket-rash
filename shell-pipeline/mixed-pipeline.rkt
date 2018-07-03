#lang racket/base
(require "private/mixed-pipeline.rkt")
(define run-pipeline run-mixed-pipeline)
(provide
 (all-from-out "private/mixed-pipeline.rkt")
 run-pipeline
 )
