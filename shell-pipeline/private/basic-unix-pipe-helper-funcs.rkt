#lang racket/base

(provide (for-syntax basic-unix-pipe-transformer))

(require
 racket/match
 "mostly-structs.rkt"
 (submod "subprocess-pipeline.rkt" resolve-command-path)
 racket/list
 (for-syntax
  racket/base
  syntax/parse
  syntax/keyword
  "misc-utils.rkt"
  "filter-keyword-args.rkt"
  ))

;; This is to evaluate the command form before any arguments, so I can raise
;; an error early if the command doesn't exist.  This provides better error
;; messages for eg. line-macro names that are misspelled or not required.
(define-syntax (unix-args-eval stx)
  (syntax-parse stx
    [(_ arg ...)
     #'(unix-args-eval-func (list (λ () arg) ...))]))
(define (unix-args-eval-func arg-thunks)
  (reverse
   (for/fold ([argl-rev '()])
             ([t arg-thunks])
     (define cur-arg (t))
     (if (null? argl-rev)
         (match (unix-args-eval-match cur-arg)
           [(list) argl-rev]
           [x (cons x argl-rev)])
         (cons cur-arg argl-rev)))))
(define (unix-args-eval-match cmd-arg)
  (define (resolve c)
    (cond [(or (path? c) (string? c) (symbol? c))
           (resolve-command-path c)]
          [(or (procedure? c) (prop:alias-func? c)) c]
          [else (error '=basic-unix-pipe=
                       "bad command, expected a path/string/symbol or function: ~a"
                       c)]))
  (match cmd-arg
    [(list a ...)
     (define flat-cmdlist (flatten a))
     (match flat-cmdlist
       [(list cmd arg ...)
        (cons (resolve cmd) arg)]
       [(list) (list)])]
    [cmd (resolve cmd)]))

(define-for-syntax (basic-unix-pipe/ordered-args stx)
  (syntax-parse stx
    [(arg-maybe-opt ...+)
     (define-values (opts rest-stx)
       (parse-keyword-options #'(arg-maybe-opt ...)
                              (list
                               (list '#:err check-expression)
                               (list '#:env check-expression)
                               (list '#:success check-expression)
                               )
                              #:no-duplicates? #t))
     (syntax-parse rest-stx
       [(arg ...)
        (let ([success-pred (opref opts '#:success #'(pipeline-default-option))]
              ;; TODO - hook up env
              [env-extend (opref opts '#:env #''())]
              [err (or (opref opts '#:err #f)
                       #'(pipeline-default-option))])
          #`(unix-pipeline-member-spec (flatten (unix-args-eval arg ...))
                                       #:err #,err
                                       #:success #,success-pred))])]))

(define-for-syntax (basic-unix-pipe-transformer stx)
  (syntax-parse stx
    [(_ arg ...+)
     (let-values ([(kwargs pargs) (filter-keyword-args #'(arg ...))])
       (basic-unix-pipe/ordered-args
        (datum->syntax #f (append kwargs pargs))))]))
