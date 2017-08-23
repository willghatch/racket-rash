#lang racket/base

(provide
 set-default-line-macro!
 with-default-line-macro
 splicing-with-default-line-macro
 (for-syntax
  get-default-line-macro
  ))

(module+ for-public
  (provide
   set-default-line-macro!
   ))

(require
 shell/private/settable-lexical-default
 "line-macros.rkt"
 )

(define-settable-lexical-default
  default-line-macro
  pipeline-line-macro)
