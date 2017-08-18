#lang racket/base

(module+ for-public
  (provide
   do-line-macro
   ))

(provide
 rash-read-and-line-parse
 rash-line-parse
 )


(require
 "pipeline-macro-parse.rkt"
 "read-funcs.rkt"
 syntax/parse
 racket/stxparam
 racket/splicing
 racket/string
 racket/port
 shell/mixed-pipeline
 "pipeline-operator-default.rkt"
 "pipeline-operators.rkt"
 "pipeline-operator-transform.rkt"
 "line-macro-default.rkt"
 (for-syntax
  racket/base
  syntax/parse
  racket/stxparam-exptime
  syntax/keyword
  racket/dict
  "pipeline-operator-detect.rkt"
  "line-macro-detect.rkt"
  "misc-utils.rkt"
  (for-syntax
   racket/base
   syntax/parse
   )))



(define (rash-read-and-line-parse src in)
  (let ([stx (rash-read-syntax src in)])
    (if (eof-object? stx)
        stx
        (syntax-parse stx
          [e #'(rash-set-defaults ((current-input-port)
                                   (current-output-port)
                                   (current-error-port))
                                  (rash-line-parse e))]))))

(define-syntax (rash-line-parse stx)
  (syntax-parse stx
    [(rlp arg ...)
     (syntax-parse #'(arg ...)
       #:datum-literals (%%rash-racket-line %%rash-line-start)
       [((%%rash-line-start arg ...) post ...+)
        #'(begin (do-line-macro arg ...)
                 (rlp post ...))]
       [((%%rash-line-start arg ...))
        #'(do-line-macro arg ...)]
       [((%%rash-racket-line arg ...) post ...+)
        #'(begin arg ...
                 (rlp post ...))]
       [((%%rash-racket-line arg ...))
        #'(begin arg ...)]
       [() #'(void)])]))

(define-syntax (do-line-macro stx)
  ;; detect line macros and apply them, or transform into pipeline
  (syntax-parse stx
    [(_ arg1:line-macro arg ...)
     (rash-line-macro-transform #'(arg1 arg ...))]
    [(_ arg ...)
     (rash-line-macro-transform #`(#,(get-default-line-macro) arg ...))]))

