#lang racket/base

(require "mixed-pipeline.rkt")

(define pline
  (-run-pipeline
   (list
    (obj-pipeline-member-spec (λ () "testing\nhello\nrunning"))
    (obj-pipeline-member-spec (λ (arg) (string-upcase arg)))
    ;(u-pipeline-member-spec (list "grep" "-i" "ing") 'null)
    (composite-pipeline-member-spec
     (list
      (u-pipeline-member-spec (list "/usr/bin/grep" "-i" "ing") 'null)
      (u-pipeline-member-spec (list "grep" "-i" "ing") 'null)))
    )
   #f
   (current-output-port)))

(pipeline-wait pline)
(pipeline-success? pline)
(pipeline-ret pline)
