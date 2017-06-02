#lang racket/base

(provide
 rash
 rash/wired
 ;; TODO - what should be provided from the pipeline libraries?
 (all-from-out shell/pipeline)
 ;define-alias

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
  (for-syntax
   racket/base
   syntax/parse
   )
  syntax/keyword
  racket/dict
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


(define (rash-read-and-line-parse src in)
  (let ([stx (rash-read-syntax src in)])
    (if (eof-object? stx)
        stx
        (syntax-parse stx
          [e #'(rash-line-parse ((current-input-port)
                                 (current-output-port)
                                 (current-error-port))
                                e)]))))

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

(begin-for-syntax
  (define rash-keyword-table
    (list (list '#:in check-expression)
          (list '#:out check-expression)
          (list '#:err check-expression)))
  (define (opref table key default)
    (with-handlers ([(λ _ #t) (λ (e) default)])
      (cadr (dict-ref table key))))
  )

(define-syntax (rash stx)
  (syntax-parse stx
    [(rash orig-arg ...)
     (define-values (tab rest-stx)
       (parse-keyword-options #'(orig-arg ...)
                              rash-keyword-table
                              #:context stx
                              #:no-duplicates? #t))
     (define code-str-stx
       (syntax-parse rest-stx
         [(rash-src:str) #'rash-src]
         [(src-seg:str ...+) (scribble-strings->string #'(src-seg ...))]))

     (with-syntax ([(parsed-rash-code ...)
                    (map (λ (s) (replace-context code-str-stx s))
                         (syntax->list
                          (rash-read-syntax-all (syntax-source code-str-stx)
                                                (stx-string->port code-str-stx))))]
                   [input (opref tab '#:in #'(open-input-string ""))]
                   [output (opref tab '#:out #'default-output-port-transformer)]
                   [err-output (opref tab '#:err #''string-port)])
       (let* ([implicit-key (gensym 'rash-implicit-starter-key-)]
              [set (hash-set! implicit-pipe-starter-hash
                              implicit-key
                              (hash-ref implicit-pipe-starter-hash
                                        {syntax-parameter-value
                                         #'implicit-pipe-starter-key}))])
         #`(let ([in-eval input]
                 [out-eval output]
                 [err-eval err-output])
             (syntax-parameterize ([implicit-pipe-starter-key
                                    (quote #,(datum->syntax #'here implicit-key))])
               (rash-line-parse (in-eval out-eval err-eval) parsed-rash-code ...)))))]))

(define-syntax (rash/wired stx)
  (syntax-parse stx
    [(_ arg ...)
     (define-values (tab rest-stx)
       (parse-keyword-options #'(arg ...)
                              rash-keyword-table
                              #:context stx
                              #:no-duplicates? #t))
     (with-syntax ([(code-segs-hopefully ...) rest-stx]
                   [in (opref tab '#:in #'(current-input-port))]
                   [out (opref tab '#:out #'(current-output-port))]
                   [err (opref tab '#:err #'(current-error-port))])
       #'(rash #:in in #:out out #:err err code-segs-hopefully ...))]))

