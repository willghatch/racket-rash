#lang racket/base

(provide
 (all-from-out (submod "private/pipeline-macro-parse.rkt" for-public))
 (all-from-out "private/pipeline-operator-default.rkt")
 (all-from-out "private/pipeline-operators.rkt")
 (all-from-out "private/good-unix-operator.rkt")
 (all-from-out "private/define-pipeline-alias.rkt")
 (all-from-out "private/define-simple-alias.rkt")
 (all-from-out "private/pipeline-macro-logicwrapper.rkt")
 (except-out (all-from-out "mixed-pipeline.rkt")
             and/success
             or/success)

 ;; Short names
 \|
 \|>
 \|>>
 _

 &&
 \|\|

 ;; TODO - some things from pipeline-operator-detect.rkt should be provided
 (for-syntax
  pipeline-starter
  pipeline-joint
  ))
(require
 (submod "private/pipeline-macro-parse.rkt" for-public)
 "private/pipeline-operator-default.rkt"
 ;(for-syntax "private/pipeline-operator-detect.rkt")
 "private/pipeline-operators.rkt"
 "private/good-unix-operator.rkt"
 "private/define-pipeline-alias.rkt"
 "private/define-simple-alias.rkt"
 "private/pipeline-macro-logicwrapper.rkt"
 (except-in "mixed-pipeline.rkt"
            run-pipeline)

 (for-syntax
  racket/base
  ))

(define-syntax \| (make-rename-transformer #'=unix-pipe=))
(define-syntax \|> (make-rename-transformer #'=basic-object-pipe=))
(define-syntax \|>> (make-rename-transformer #'=object-pipe=))
(define-syntax _ (make-rename-transformer #'current-pipeline-argument))

(define-syntax && (make-rename-transformer #'and/success))
(define-syntax \|\| (make-rename-transformer #'or/success))
