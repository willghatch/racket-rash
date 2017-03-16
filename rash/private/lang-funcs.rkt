#lang racket/base

(provide
 rash
 rash/out
 rash/trim
 rash/number
 (all-from-out shell/pipeline)
 rash-splice
 )

(module+ for-module-begin
  (provide rash-line-parse
           rash-read-and-line-parse
           ))

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/strip-context
                     udelim
                     ))
(require racket/string)
(require syntax/parse)
(require racket/port)
(require shell/pipeline)
(require "read-funcs.rkt")
(require (for-syntax "read-funcs.rkt"))


(struct rash-splice
  ;; struct for signalling that a pipeline-member argument needs to be
  ;; spliced into the argument list
  (content))

(define (run-pipeline/splice #:out [out (current-output-port)] . members)
  (define (do-splice inlist outlist)
    (cond [(null? inlist) (reverse outlist)]
          [(rash-splice? (car inlist))
           (let* ([content (rash-splice-content (car inlist))]
                  [cspliced (if (list? content)
                                (reverse content)
                                (list content))])
             (do-splice (cdr inlist) (append cspliced outlist)))]
          [else (do-splice (cdr inlist) (cons (car inlist) outlist))]))
  ;; All pipeline members that we get in rash are lists.
  (define (splice-member m)
    (do-splice m '()))
  (apply run-pipeline (map splice-member members) #:out out))


(define (rash-read-and-line-parse src in)
  (let ([stx (rash-read-syntax src in)])
    (if (eof-object? stx)
        stx
        (syntax-parse stx
          [e #'(rash-line-parse e)]))))

(begin-for-syntax
  (define-syntax-class not-pipe
    (pattern (~not (~literal \|))))
  (define-splicing-syntax-class pipeline-part
    (pattern (~seq arg:not-pipe ...+)
             #:attr argv #`(list #,@(map quote-maybe (syntax->list #'(arg ...))))
             ))
  (define-splicing-syntax-class pipeline-part/not-first
    (pattern (~seq (~and (~var pipe) (~literal \|)) part:pipeline-part)
             #:attr argv #`(list #,@(map quote-maybe (syntax->list #'(part.arg ...))))
             #:attr pipe-char #`pipe
             ))
  (define (quote-maybe stx)
    (syntax-parse stx
      #:datum-literals (%%rash-dispatch-marker %%rash-dispatch-marker-splice)
      [(%%rash-dispatch-marker s) #'s]
      [(%%rash-dispatch-marker-splice s) #'(rash-splice s)]
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
    #:datum-literals (%%rash-newline-symbol %%rash-racket-line %%rash-line-start)
    [(rash-line-parse (%%rash-line-start arg ...) post ...+)
     #'(begin (rash-line arg ...)
              (rash-line-parse post ...))]
    [(rash-line-parse (%%rash-line-start arg ...))
     #'(rash-line arg ...)]
    [(rash-line-parse (%%rash-racket-line arg ...) post ...+)
     #'(begin arg ...
              (rash-line-parse post ...))]
    [(rash-line-parse (%%rash-racket-line arg ...))
     #'(begin arg ...)]
    [(rash-line-parse) #'(void)]
    ))

(define-syntax (rash-line stx)
  (syntax-parse stx
    #:datum-literals (> >! >>)
    [(shell-line p1:pipeline-part pn:pipeline-part/not-first ... > filename)
     (with-disappeared-uses
       (record-disappeared-uses #'(pn.pipe-char ...))
       #`(run-pipeline/splice #:out (list #,(quote-maybe #'filename) 'error)
                              p1.argv pn.argv ...))]
    [(shell-line p1:pipeline-part pn:pipeline-part/not-first ... >! filename)
     #`(run-pipeline/splice #:out (list #,(quote-maybe #'filename) 'truncate)
                            p1.argv pn.argv ...)]
    [(shell-line p1:pipeline-part pn:pipeline-part/not-first ... >> filename)
     #`(run-pipeline/splice #:out (list #,(quote-maybe #'filename) 'append)
                            p1.argv pn.argv ...)]
    [(shell-line p1:pipeline-part pn:pipeline-part/not-first ...)
     #'(run-pipeline/splice p1.argv pn.argv ...)]
    ))

(define-syntax (rash stx)
  (syntax-parse stx
    [(rash arg:str)
     (with-syntax ([(parg ...) (map (λ (s) (replace-context #'arg s))
                                    (syntax->list
                                     (rash-read-syntax-all (syntax-source #'arg)
                                                           (stx-string->port #'arg))))])
       #'(rash-line-parse parg ...))]
    [(rash arg:str ...+)
     (with-syntax ([one-str (scribble-strings->string #'(arg ...))])
       #'(rash one-str))]))

(define-syntax (rash/out stx)
  (syntax-parse stx
    [(rash arg:str)
     (with-syntax ([(parg ...) (map (λ (s) (replace-context #'arg s))
                                    (syntax->list
                                     (rash-read-syntax-all (syntax-source #'arg)
                                                           (stx-string->port #'arg))))])
       #'(let* ([out (open-output-string)]
                [err (open-output-string)]
                [in (open-input-string "")])
           (parameterize ([current-output-port out]
                          [current-error-port err]
                          [current-input-port in])
             (let ([pline (rash-line-parse parg ...)])
               (if (pipeline-success? pline)
                   (get-output-string out)
                   (error 'rash/out
                          (string-append
                           "pipeline error:~n"
                           "pipeline member exited with: ~a~n"
                           "stderr:~n~a~n")
                          (pipeline-status pline)
                          (get-output-string err)))))))]
    [(rash arg:str ...+)
     (with-syntax ([one-str (scribble-strings->string #'(arg ...))])
       #'(rash one-str))]))

(define-syntax (rash/trim stx)
  (syntax-parse stx
    [(r/t arg:str ...+)
     #'(string-trim (rash/out arg ...))]))
(define-syntax (rash/number stx)
  (syntax-parse stx
    [(r/t arg:str ...+)
     #'(string->number (string-trim (rash/out arg ...)))]))


