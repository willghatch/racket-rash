#lang racket/base

(provide
 rash-line-parse
 rash-line
 rash
 rash/out
 rash/values
 (all-from-out "shell-funcs.rkt")
 )

(require (for-syntax racket/base
                     syntax/parse
                     ))
(require racket/port)
(require "shell-funcs.rkt")
(require "read-funcs.rkt")
(require (for-syntax "read-funcs.rkt"))

(begin-for-syntax
  (define-syntax-class not-pipe
    (pattern (~not (~literal \|))))
  (define-splicing-syntax-class pipeline-part
    (pattern (~seq arg:not-pipe ...+)
             #:attr argv #`(list #,@(map quote-maybe (syntax->list #'(arg ...))))
             ))
  (define-splicing-syntax-class pipeline-part/not-first
    (pattern (~seq (~literal \|) part:pipeline-part)
             #:attr argv #`(list #,@(map quote-maybe (syntax->list #'(part.arg ...))))
             ))
  (define (quote-maybe stx)
    (syntax-parse stx
      [x:id
       (if (syntax-property stx 'rash-mark-for-quoting)
           #'(quote x)
           #'x)]
      [else stx]))

  )


(define-syntax (rash-line-parse stx)
  (define-syntax-class not-newline
    (pattern (~not (~literal %%rash-newline-symbol))))
  (syntax-parse stx
    #:datum-literals (%%rash-newline-symbol)
    ;; strip any newlines at the beginning
    [(rash-line-parse %%rash-newline-symbol post ...)
     #'(rash-line-parse post ...)]
    ;; strip any newlines at the end
    [(rash-line-parse pre ... %%rash-newline-symbol)
     #'(rash-line-parse pre ...)]
    ;; not last line
    [(rash-line-parse pre:not-newline ...+ %%rash-newline-symbol post ...+)
     #'(begin (rash-line pre ...) (rash-line-parse post ...))]
    ;; last line
    [(rash-line-parse ll:not-newline ...+)
     #'(rash-line ll ...)]
    ))

(define-syntax (rash-line stx)
  (syntax-parse stx
    #:datum-literals (&)
    [(shell-line & form ...)
     #'(begin form ...)]
    [(shell-line p1:pipeline-part pn:pipeline-part/not-first ...)
     #'(run-pipeline p1.argv pn.argv ...)]
    ))

(define-syntax (rash stx)
  (syntax-parse stx
    [(rash arg ...+)
     (with-syntax ([(parg ...) (rash-parse-at-reader-output (syntax->list #'(arg ...)))])
       #'(rash-line-parse parg ...))]))

(define-syntax (rash/out stx)
  (syntax-parse stx
    [(rash arg ...+)
     (with-syntax ([(parg ...) (rash-parse-at-reader-output (syntax->list #'(arg ...)))])
       #'(let* ([out (open-output-string)]
                [err (open-output-string)]
                [in (open-input-string "")])
           (parameterize ([current-output-port out]
                          [current-error-port err]
                          [current-input-port in])
             (let ([ret-val (rash-line-parse parg ...)])
               (if (equal? ret-val 0)
                   (get-output-string out)
                   (error 'rash/out
                          "non-zero exit (~a) with stderr: ~a"
                          ret-val
                          (get-output-string err)))))))]))

(define-syntax (rash/values stx)
  (syntax-parse stx
    [(rash arg ...+)
     (with-syntax ([(parg ...) (rash-parse-at-reader-output (syntax->list #'(arg ...)))])
       #'(let* ([out (open-output-string)]
                [err (open-output-string)]
                [in (open-input-string "")])
           (parameterize ([current-output-port out]
                          [current-error-port err]
                          [current-input-port in])
             (let ([ret-val (rash-line-parse parg ...)])
               (values ret-val (get-output-string out) (get-output-string err))))))]))


