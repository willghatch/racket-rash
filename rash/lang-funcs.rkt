#lang racket/base

(provide
 shell-line-parse
 shell-line
 )

(require (for-syntax racket/base
                     syntax/parse
                     ))
(require "shell-funcs.rkt")

(begin-for-syntax
  (define-syntax-class not-pipe
    (pattern (~not (~literal \|))))
  (define-splicing-syntax-class pipeline-part
    (pattern (~seq cmd:not-pipe arg:not-pipe ...)
             #:attr argv #`(list #,(quote-if-id #'cmd)
                                 #,@(quote-if-id #'(arg ...)))
             ))
  (define-splicing-syntax-class pipeline-part/not-first
    (pattern (~seq (~literal \|) part:pipeline-part)
             #:attr argv #`(list #,(quote-if-id #'part.cmd)
                                 #,@(quote-if-id #'(part.arg ...)))
             ))
  (define (quote-if-id stx)
    (syntax-parse stx
      [() '()]
      [(x:id xs ...) (cons (quote-if-id #'x) (quote-if-id #'(xs ...)))]
      [x:id #'(quote x)]
      [else stx]))
  )


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
  (syntax-parse stx
    [(shell-line) #'(void)]
    [(shell-line p1:pipeline-part pn:pipeline-part/not-first ...)
     #'(rash-pipeline p1.argv pn.argv ...)]))



#;(module+ main
  (shell-line-parse this is a test %%read-newline-symbol to see what happens))
