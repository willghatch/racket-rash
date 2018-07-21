#lang racket/base

(provide
 current-rash-top-level-print-formatter
 )
(module+ default-rash-formatter
  (provide default-rash-formatter))

(require
 shell/mixed-pipeline
 racket/exn
 )

(define (default-rash-formatter last-ret)
  (cond
    [(void? last-ret) ""]
    [(exn? last-ret)
     (exn-message last-ret)]
    ;; TODO - it would be good to give stack traces *except* for
    ;; errors originating in Rash.  Maybe if shell-pipeline has its own
    ;; exn types?
    [else (format "~s" last-ret)]))

(define current-rash-top-level-print-formatter (make-parameter default-rash-formatter))
