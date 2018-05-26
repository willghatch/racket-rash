#lang racket/base

(provide
 (all-from-out rash/demo/setup)

 (except-out (all-from-out racket/base) #%module-begin)
 (rename-out [basic-rash-module-begin #%module-begin])
 (all-from-out rash)

 app
 def
 )

(require
 rash/demo/setup

 rash
 (for-syntax
  racket/base
  syntax/parse
  ))

(define-rash-module-begin basic-rash-module-begin
  #:in (current-input-port)
  #:out (current-output-port)
  #:err (current-error-port)
  #:default-starter =quoting-basic-unix-pipe=
  )
#;(define-syntax basic-rash-module-begin
  (make-rash-module-begin-transformer #:in (current-input-port)
                                      #:out (current-output-port)
                                      #:err (current-error-port)
                                      #:default-starter #'=quoting-basic-unix-pipe=
                                      ;#:default-line-macro #'pipeline-line-macro
                                      ))


;;; additional demo definitions

(define-line-macro app (syntax-parser [(_ arg ...) #'(arg ...)]))
(define-line-macro def (syntax-parser [(_ def-form arg ...)
                                       #'(define def-form
                                           (do-line-macro arg ...))]))

