#lang racket/base

(provide
 (all-from-out rash/demo/setup)

 (except-out (all-from-out racket/base) #%module-begin)
 (rename-out [basic-rash-module-begin #%module-begin])
 (all-from-out rash)
 )

(require
 rash/demo/setup

 rash
 (submod rash/private/lang-funcs for-module-begin)
 (for-syntax
  racket/base))

(define-syntax basic-rash-module-begin
  (make-rash-module-begin-transformer #:in (current-input-port)
                                      #:out (current-output-port)
                                      #:err (current-error-port)
                                      #:default-starter #'=quoting-basic-unix-pipe=
                                      ;#:default-line-macro #'pipeline-line-macro
                                      ))
