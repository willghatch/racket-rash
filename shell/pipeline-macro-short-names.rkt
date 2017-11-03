#lang racket/base

(provide (all-defined-out))

(require 
 "pipeline-macro.rkt"
 (for-syntax
  racket/base
  ))

;; UNSTABLE short names

(define-syntax \| (make-rename-transformer #'=default-unix-pipe=))
(define-syntax \|> (make-rename-transformer #'=object-pipe=))
(define-syntax _ (make-rename-transformer #'current-pipeline-argument))
