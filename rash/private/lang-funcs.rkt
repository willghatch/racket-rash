#lang racket/base

(provide
 rash
 rash/wired
 ;; TODO - what should be provided from the pipeline libraries?
 (all-from-out shell/mixed-pipeline)
 ;define-alias
 )


(module+ for-module-begin
  (provide rash-line-parse
           rash-read-and-line-parse
           rash-pipeline-opt-hash
           default-output-port-transformer
           (for-syntax implicit-pipe-starter-hash)
           implicit-pipe-starter-key
           ))


(require
 racket/stxparam
 racket/string
 syntax/parse
 racket/port
 ;shell/pipeline
 shell/mixed-pipeline
 "read-funcs.rkt"
 "parse-funcs.rkt"

 (for-syntax
  syntax/keyword
  racket/dict
  racket/base
  racket/match
  racket/syntax
  racket/string
  racket/stxparam-exptime
  syntax/parse
  syntax/strip-context
  udelim
  "read-funcs.rkt"

  (for-syntax
   racket/base
   syntax/parse
   )))



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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TODO - below is "macro-detect", but I've merged the two files.  I want to organize them, and maybe break out new files.






