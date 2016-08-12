#lang racket/base

(provide
 shell-line-parse
 shell-line
 )

(require (for-syntax racket/base
                     syntax/parse
                     ))
(require "shell-funcs.rkt")


(define-syntax (shell-line-parse stx)
  (define-syntax-class not-newline
    (pattern (~not (~literal %%read-newline-symbol))))
  (syntax-parse stx
    #:datum-literals (%%read-newline-symbol)
    [(shell-line-parse %%read-newline-symbol post ...)
     #'(shell-line-parse post ...)]
    [(shell-line-parse pre:not-newline ... %%read-newline-symbol post ...)
     #'(begin (shell-line pre ...) (shell-line-parse post ...))]
    [(shell-line-parse foo:not-newline ...)
     #'(shell-line foo ...)]))

(define-syntax (shell-line stx)
  (define-syntax-class not-pipe
    (pattern (~not (~literal \|))))
  (syntax-parse stx
    #:datum-literals (\|)
    [(shell-line) #'(void)]
    [(shell-line cmd:not-pipe arg:not-pipe ...)
     #'(rash-pipeline (quote (cmd arg ...)))]
    [(shell-line cmd1:not-pipe arg1:not-pipe ... \| cmd2:not-pipe arg2:not-pipe ...)
     #'(rash-pipeline (quote (cmd1 arg1 ...)) (quote (cmd2 arg2 ...)))
     ]))



#;(module+ main
  (shell-line-parse this is a test %%read-newline-symbol to see what happens))
