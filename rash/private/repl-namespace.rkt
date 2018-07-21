#lang rash

(provide
 set-default-pipeline-starter!
 repl-default-pipeline-starter
 set-default-line-macro!
 repl-default-line-macro
 )

(require
 (for-syntax
  racket/base
  syntax/parse
  ))


(define-for-syntax repl-default-pipeline-starter-variable #'=unix-pipe=)
(define-line-macro set-default-pipeline-starter!
  (syntax-parser [(_ starter)
                  (set! repl-default-pipeline-starter-variable #'starter)
                  #'(void)]))

(define-pipeline-operator
  repl-default-pipeline-starter
  #:start (syntax-parser [(_ arg ...)
                          #`(#,repl-default-pipeline-starter-variable arg ...)]))

(define-for-syntax repl-default-line-macro-variable #'run-pipeline/logic)
(define-line-macro set-default-line-macro!
  (syntax-parser [(_ lm)
                  (set! repl-default-line-macro-variable #'lm)
                  #'(void)]))

(define-line-macro
  repl-default-line-macro
  (syntax-parser [(_ arg ...)
                  #`(#,repl-default-line-macro-variable arg ...)]))

