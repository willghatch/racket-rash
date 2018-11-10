#lang racket/base

(require "../../mixed-pipeline.rkt")

(module+ test
  (require
   rackunit
   "test-pipeline.rkt"
   racket/string
   )


  (check-equal?
   (run-pipeline
    (object-pipeline-member-spec (λ () "testing\nhello\nrunning"))
    (object-pipeline-member-spec (λ (arg) (string-upcase arg)))
    ;(u-pipeline-member-spec (list "grep" "-i" "ing") null-redirect)
    (composite-pipeline-member-spec
     (list
      (unix-pipeline-member-spec (list my-grep "ING") #:err null-redirect)
      (unix-pipeline-member-spec (list my-grep "EST") #:err null-redirect))))
   "TESTING")

  )

