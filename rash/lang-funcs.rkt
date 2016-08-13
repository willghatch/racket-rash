#lang racket/base

(provide
 rash-line-parse
 rash-line
 rash-read-syntax
 rash-read
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
(require "readtable.rkt")

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


(define-syntax (rash-line-parse stx)
  (define-syntax-class not-newline
    (pattern (~not (~literal %%read-newline-symbol))))
  (syntax-parse stx
    #:datum-literals (%%read-newline-symbol)
    [(rash-line-parse %%read-newline-symbol post ...)
     #'(rash-line-parse post ...)]
    [(rash-line-parse pre:not-newline ... %%read-newline-symbol post ...)
     #'(begin (rash-line pre ...) (rash-line-parse post ...))]
    [(rash-line-parse foo:not-newline ...)
     #'(rash-line foo ...)]))

(define-syntax (rash-line stx)
  (syntax-parse stx
    [(shell-line) #'(void)]
    [(shell-line p1:pipeline-part pn:pipeline-part/not-first ...)
     #'(rash-pipeline p1.argv pn.argv ...)]))

(define-syntax (rash stx)
  (syntax-parse stx
    [(rash arg ...+)
     (with-syntax ([(parg ...) (parse-at-reader-output (syntax->list #'(arg ...)))])
       #'(rash-line-parse parg ...))]))

(define-syntax (rash/out stx)
  (syntax-parse stx
    [(rash arg ...+)
     (with-syntax ([(parg ...) (parse-at-reader-output (syntax->list #'(arg ...)))])
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
     (with-syntax ([(parg ...) (parse-at-reader-output (syntax->list #'(arg ...)))])
       #'(let* ([out (open-output-string)]
                [err (open-output-string)]
                [in (open-input-string "")])
           (parameterize ([current-output-port out]
                          [current-error-port err]
                          [current-input-port in])
             (let ([ret-val (rash-line-parse parg ...)])
               (values ret-val (get-output-string out) (get-output-string err))))))]))

(begin-for-syntax
  (define (parse-at-reader-output argl)
    (for/fold ([out-list '()])
              ([lpart argl])
      (if (string? (syntax->datum lpart))
          (append
           out-list
           (rash-read-syntax-seq #f (open-input-string (syntax->datum lpart))))
          (append out-list (list lpart)))))

  (define (rash-read-syntax-seq src in)
    (let ([result (rash-read-syntax src in)])
      (if (equal? eof result)
          '()
          (cons result (rash-read-syntax-seq src in)))))


  )



