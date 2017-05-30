#lang racket/base

(provide
 rash
 rash/out
 rash/trim
 rash/number
 (all-from-out shell/pipeline)
 rash-splice
 rash-splice?
 define-alias

 ;; pipe operator detection
 =basic-object-pipe=
 =crappy-basic-unix-pipe=
 =obj=
 default-pipe-starter!
 )

(module+ for-module-begin
  (provide rash-line-parse
           rash-read-and-line-parse
           ))

(require
 (for-syntax
  racket/base
  racket/syntax
  syntax/parse
  syntax/strip-context
  udelim
  )
 "macro-detect.rkt"
 )

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
  (define (quote-argv argv-stx)
    (syntax-parse argv-stx
      [(cmd arg ...)
       #`(list #,(quote-maybe-cmd #'cmd)
               #,@(map quote-maybe (syntax->list #'(arg ...))))]))
  (define-splicing-syntax-class pipeline-part
    (pattern (~seq arg:not-pipe ...+)
             #:attr argv (quote-argv #'(arg ...))
             ))
  (define-splicing-syntax-class pipeline-part/not-first
    (pattern (~seq (~and (~var pipe) (~literal \|)) part:pipeline-part)
             #:attr argv (quote-argv #'(part.arg ...))
             #:attr pipe-char #`pipe
             ))

  (define (quote-maybe stx)
    (syntax-parse stx
      #:datum-literals (%%rash-dispatch-marker %%rash-dispatch-marker-splice)
      [(%%rash-dispatch-marker s) #'s]
      [(%%rash-dispatch-marker-splice s) #'(rash-splice s)]
      [x:id #'(quote x)]
      [else stx]))
  (define (quote-maybe-cmd stx)
    (let ([slv (and (identifier? stx)
                    ;; TODO I need the third argument of this to be true for
                    ;; the repl to work.  But I worry that it could break
                    ;; something for non-top-level code...
                    (identifier-binding stx (syntax-local-phase-level) #t)
                    (let-values ([(transform-val normal-val)
                                  (syntax-local-value/immediate stx
                                                                (λ _ (values #f #f)))])
                      normal-val))])
      (if (and (syntax? slv) (identifier? slv)
               (syntax-property slv 'rash-alias-identifier?))
          stx
          (quote-maybe stx))))

  )

(define-syntax (define-alias stx)
  (syntax-parse stx
    [(_ name:id arglist body ...+)
     (with-syntax ([defname (datum->syntax stx (gensym (syntax->datum #'name)))])
       #'(begin
           ;; define a temp name so val is only evaluated once
           (define defname (alias-func (λ arglist body ...)))
           (define-syntax name (make-rename-transformer
                                (syntax-property
                                 (syntax-property #'defname
                                                  'rash-alias-identifier?
                                                  #t #t)
                                 ;; this not-free-identifier=? makes it export
                                 ;; name instead of defname when provided.
                                 'not-free-identifier=? #t #t)))))]))


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
    [(_ arg ...+) #'(rash-pipeline-splitter arg ...)]))

#;(define-syntax (rash-line stx)
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


