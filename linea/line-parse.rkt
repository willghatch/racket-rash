#lang racket/base

(provide
 linea-line-parse
 do-line-macro
 )

(require
 "read-funcs.rkt"
 syntax/parse
 racket/stxparam
 racket/splicing
 racket/string
 racket/port
 shell/mixed-pipeline
 "line-macro-default.rkt"
 (for-syntax
  racket/base
  syntax/parse
  racket/stxparam-exptime
  syntax/keyword
  racket/dict
  "line-macro-detect.rkt"
  shell/private/misc-utils
  (for-syntax
   racket/base
   syntax/parse
   )))

(define-syntax (linea-line-parse stx)
  (syntax-parse stx
    [(rlp arg ...)
     (syntax-parse #'(arg ...)
       #:datum-literals (%%linea-racket-line %%linea-line-start)
       [((%%linea-line-start arg ...) post ...+)
        #'(begin (do-line-macro arg ...)
                 (rlp post ...))]
       [((%%linea-line-start arg ...))
        #'(do-line-macro arg ...)]
       [((%%linea-racket-line arg ...) post ...+)
        #'(begin arg ...
                 (rlp post ...))]
       [((%%linea-racket-line arg ...))
        #'(begin arg ...)]
       [() #'(void)])]))

(define-syntax (do-line-macro stx)
  ;; detect line macros and apply them, or transform into pipeline
  (syntax-parse stx
    [(_ arg1:line-macro arg ...)
     (linea-line-macro-transform #'(arg1 arg ...))]
    [(_ arg ...)
     (linea-line-macro-transform #`(#,(get-default-line-macro) arg ...))]))
