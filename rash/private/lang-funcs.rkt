#lang racket/base

(provide
 rash
 (all-from-out shell/pipeline)
 rash-splice
 rash-splice?
 define-alias

 (except-out (all-from-out "macro-detect.rkt")
             rash-pipeline-opt-hash
             rash-pipeline-splitter
             default-output-port-transformer
             (for-syntax implicit-pipe-starter-hash)
             implicit-pipe-starter-key
             ))

(module+ for-module-begin
  (provide rash-line-parse
           rash-read-and-line-parse
           rash-pipeline-opt-hash
           default-output-port-transformer
           (for-syntax implicit-pipe-starter-hash)
           implicit-pipe-starter-key
           ))

(require
 (for-syntax
  (for-syntax racket/base syntax/parse)
  racket/base
  racket/syntax
  racket/stxparam-exptime
  syntax/parse
  syntax/strip-context
  udelim
  )
 racket/stxparam
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
          [e #'(rash-line-parse ((current-input-port)
                                 (current-output-port)
                                 (current-error-port))
                                e)]))))

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
    [(rlp (in out err) arg ...)
     (with-syntax ([ioe #'(in out err)])
       (syntax-parse #'(arg ...)
         #:datum-literals (%%rash-newline-symbol %%rash-racket-line %%rash-line-start)
         [((%%rash-line-start arg ...) post ...+)
          #'(begin (rash-pipeline-splitter ioe arg ...)
                   (rlp ioe post ...))]
         [((%%rash-line-start arg ...))
          #'(rash-pipeline-splitter ioe arg ...)]
         [((%%rash-racket-line arg ...) post ...+)
          #'(begin arg ...
                   (rlp ioe post ...))]
         [((%%rash-racket-line arg ...))
          #'(begin arg ...)]
         [() #'(void)]
         ))])
  )

(define-syntax (rash stx)
  (syntax-parse stx
    [(rash (~or (~optional (~seq #:in input:expr)
                           #:name "#:in option"
                           #:defaults ([input #'(open-input-string "")]))
                (~optional (~seq #:out output:expr)
                           #:name "#:out option"
                           #:defaults ([output #'default-output-port-transformer]))
                (~optional (~seq #:err err:expr)
                           #:name "#:err option"
                           #:defaults ([err #''string-port])))
           ...
           arg:str)
     (with-syntax ([(parg ...) (map (λ (s) (replace-context #'arg s))
                                    (syntax->list
                                     (rash-read-syntax-all (syntax-source #'arg)
                                                           (stx-string->port #'arg))))])
       (let* ([implicit-key (gensym 'rash-implicit-starter-key-)]
              [set (hash-set! implicit-pipe-starter-hash
                              implicit-key
                              (hash-ref implicit-pipe-starter-hash
                                        {syntax-parameter-value #'implicit-pipe-starter-key}))])
         #`(let ([in-eval input]
                 [out-eval output]
                 [err-eval err])
             (syntax-parameterize ([implicit-pipe-starter-key
                                    (quote #,(datum->syntax #'here implicit-key))])
               (rash-line-parse (in-eval out-eval err-eval) parg ...)))))]
    [(rash (~and ops (not opstr:str)) ... arg:str ...+)
     ;; TODO - deal with opts better
     (with-syntax ([one-str (scribble-strings->string #'(arg ...))])
       #'(rash ops ... one-str))]))

#;(define-syntax (rash/wired stx)
  (syntax-parse stx
    [(_)]))


