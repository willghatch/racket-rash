#lang racket/base

(provide
 (except-out (all-from-out racket/base) #%module-begin #%top-interaction)
 (rename-out [basic-rash-module-begin #%module-begin]
             [basic-rash-top-interaction #%top-interaction])
 (all-from-out rash)
 )

(require rash)

(define-rash-module-begin basic-rash-module-begin basic-rash-top-interaction
  #:this-module-path rash/private/basic-module-begin
  ;#:in (current-input-port)
  ;#:out (current-output-port)
  ;#:err (current-error-port)
  ;#:default-starter =unix-pipe=
  ;#:top-level-wrap println
  ;#:default-line-macro pipeline-line-macro
  )
