#lang racket/base

(provide
 (all-from-out (submod "private/pipeline-macro-parse.rkt" for-public))
 (all-from-out "private/pipeline-operator-default.rkt")
 (all-from-out "private/pipeline-operators.rkt")
 (all-from-out "private/good-unix-operator.rkt")
 (all-from-out "private/define-pipeline-alias.rkt")
 (all-from-out "private/define-simple-alias.rkt")
 (all-from-out "mixed-pipeline.rkt")
 ;; TODO - some things from pipeline-operator-detect.rkt should be provided
 )
(require
 (submod "private/pipeline-macro-parse.rkt" for-public)
 "private/pipeline-operator-default.rkt"
 "private/pipeline-operators.rkt"
 "private/good-unix-operator.rkt"
 "private/define-pipeline-alias.rkt"
 "private/define-simple-alias.rkt"
 (except-in "mixed-pipeline.rkt"
            run-pipeline)
 )
