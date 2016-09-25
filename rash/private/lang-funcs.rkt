#lang racket/base

(provide
 rash-line-parse
 rash-line
 rash
 rash/out
 rash/values
 rash/trim
 rash/number
 (all-from-out shell/pipeline)
 )

(require (for-syntax racket/base
                     syntax/parse
                     syntax/strip-context
                     udelim
                     ))
(require racket/string)
(require racket/port)
(require shell/pipeline)
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
      #:datum-literals (%%rash-dispatch-marker)
      [(%%rash-dispatch-marker s) #'s]
      [x:id #'(quote x)]
      [else stx]))

  )


(define-syntax (rash-line-parse stx)
  (define-syntax-class not-newline
    (pattern (~and (~not (~literal %%rash-newline-symbol))
                   (~not ((~literal %%rash-racket-line) e ...)))))
  (define-syntax-class rash-newline
    (pattern (~or (~literal %%rash-newline-symbol)
                  ((~literal %%rash-racket-line) e ...))))
  (syntax-parse stx
    #:datum-literals (%%rash-newline-symbol %%rash-racket-line)
    ;; strip any newlines at the beginning
    [(rash-line-parse %%rash-newline-symbol post ...)
     #'(rash-line-parse post ...)]
    ;; strip any newlines at the end
    [(rash-line-parse pre ... %%rash-newline-symbol)
     #'(rash-line-parse pre ...)]
    ;; if all I have is a rash-racket-line, then just do that...
    [(rash-line-parse (%%rash-racket-line e ...)) #'(begin e ...)]
    ;; take care of embedded racket lines
    [(rash-line-parse (%%rash-racket-line e ...) post ...+)
     #'(begin e ... (rash-line-parse post ...))]
    [(rash-line-parse pre ...+ (%%rash-racket-line e ...))
     #'(begin (rash-line-parse pre ...) e ...)]
    ;; not last line
    [(rash-line-parse pre:not-newline ...+ sep:rash-newline post ...+)
     #'(begin (rash-line pre ...) (rash-line-parse sep post ...))]
    ;; last line
    [(rash-line-parse ll:not-newline ...+)
     #'(rash-line ll ...)]
    ))

(define-syntax (rash-line stx)
  (syntax-parse stx
    #:datum-literals (> >! >>)
    [(shell-line p1:pipeline-part pn:pipeline-part/not-first ... > filename)
     #`(run-pipeline #:out (list #,(quote-maybe #'filename) 'error)
                     p1.argv pn.argv ...)]
    [(shell-line p1:pipeline-part pn:pipeline-part/not-first ... >! filename)
     #`(run-pipeline #:out (list #,(quote-maybe #'filename) 'truncate)
                     p1.argv pn.argv ...)]
    [(shell-line p1:pipeline-part pn:pipeline-part/not-first ... >> filename)
     #`(run-pipeline #:out (list #,(quote-maybe #'filename) 'append)
                     p1.argv pn.argv ...)]
    [(shell-line p1:pipeline-part pn:pipeline-part/not-first ...)
     #'(run-pipeline p1.argv pn.argv ...)]
    ))

(define-syntax (rash stx)
  (syntax-parse stx
    [(rash arg:str)
     (with-syntax ([(parg ...) (map (λ (s) (replace-context #'arg s))
                                    (syntax->list
                                     (rash-read-syntax* (syntax-source #'arg)
                                                        (stx-string->port #'arg))))])
       #'(rash-line-parse parg ...))]))

(define-syntax (rash/out stx)
  (syntax-parse stx
    [(rash arg:str)
     (with-syntax ([(parg ...) (map (λ (s) (replace-context #'arg s))
                                    (syntax->list
                                     (rash-read-syntax* (syntax-source #'arg)
                                                        (stx-string->port #'arg))))])
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
    [(rash arg:str)
     (with-syntax ([(parg ...) (map (λ (s) (replace-context #'arg s))
                                    (syntax->list
                                     (rash-read-syntax* (syntax-source #'arg)
                                                        (stx-string->port #'arg))))])
       #'(let* ([out (open-output-string)]
                [err (open-output-string)]
                [in (open-input-string "")])
           (parameterize ([current-output-port out]
                          [current-error-port err]
                          [current-input-port in])
             (let ([ret-val (rash-line-parse parg ...)])
               (values ret-val (get-output-string out) (get-output-string err))))))]))

(define-syntax (rash/trim stx)
  (syntax-parse stx
    [(r/t arg:str)
     #'(string-trim (rash/out arg))]))
(define-syntax (rash/number stx)
  (syntax-parse stx
    [(r/t arg:str)
     #'(string->number (string-trim (rash/out arg)))]))


