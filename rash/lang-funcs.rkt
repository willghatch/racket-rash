#lang racket/base

(provide
 shell-line-parse
 shell-line
 rash-read-syntax
 rash-read
 (all-from-out "shell-funcs.rkt")
 )

(require (for-syntax racket/base
                     syntax/parse
                     ))
(require "shell-funcs.rkt")
(require "readtable.rkt")

(define (rash-read-syntax src in)
  (parameterize ([current-readtable line-readtable])
    (read-syntax src in)))
(define (rash-read in)
  (parameterize ([current-readtable line-readtable])
    (read in)))

(begin-for-syntax
  (define-syntax-class not-pipe
    (pattern (~not (~literal \|))))
  (define-splicing-syntax-class pipeline-part
    (pattern (~seq arg:not-pipe ...+)
             #:attr argv #`(list #,@(map quote-if-id (syntax->list #'(arg ...))))
             ))
  (define-splicing-syntax-class pipeline-part/not-first
    (pattern (~seq (~literal \|) part:pipeline-part)
             #:attr argv #`(list #,@(map quote-if-id (syntax->list #'(part.arg ...))))
             ))
  (define (quote-if-id stx)
    (syntax-parse stx
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

