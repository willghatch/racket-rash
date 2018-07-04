#lang racket/base

(provide
 current-rash-top-level-print-formatter
 )

(require
 shell/mixed-pipeline
 racket/exn
 )

(define (default-rash-formatter last-ret)
  (cond
    [(void? last-ret) ""]
    [(exn? last-ret)
     (format "~a" last-ret)]
    [(and (pipeline? last-ret)
          (pipeline-running? last-ret))
     (format "~a" last-ret)]
    [(and (pipeline? last-ret)
          (not (pipeline-success? last-ret)))
     (let ([err (pipeline-return last-ret)])
       (if (exn? err)
           (exn-message err)
           (format "~s" err)))]
    [(pipeline? last-ret)
     (default-rash-formatter (pipeline-return last-ret))]
    [else (format "~s" last-ret)]))

(define current-rash-top-level-print-formatter (make-parameter default-rash-formatter))
