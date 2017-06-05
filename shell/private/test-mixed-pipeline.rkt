#lang racket/base

(require "../mixed-pipeline.rkt")

(module+ test
  (require
   rackunit
   "test-pipeline.rkt"
   racket/string
   )


  (check-equal?
   (run-pipeline
    (list
     (obj-pipeline-member-spec (λ () "testing\nhello\nrunning"))
     (obj-pipeline-member-spec (λ (arg) (string-upcase arg)))
     ;(u-pipeline-member-spec (list "grep" "-i" "ing") 'null)
     (composite-pipeline-member-spec
      (list
       (u-pipeline-member-spec (list my-grep "ING") 'null)
       (u-pipeline-member-spec (list my-grep "EST") 'null)))))
   "TESTING")

  )

