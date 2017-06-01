#lang racket/base

(provide
 (except-out (all-from-out racket/base) #%module-begin)
 (rename-out [module-begin #%module-begin])
 (all-from-out rash)
 )

(require rash
         (submod "../private/lang-funcs.rkt" for-module-begin)
         "../private/macro-detect.rkt"
         racket/splicing
         (for-syntax racket/base
                     syntax/parse
                     syntax/strip-context))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(m-b arg ...)
     (with-syntax ([(parg ...) (replace-context #'foo #'(arg ...))])
       ;; #%plain-module-begin would suppress printing...
       #'(#%module-begin
          (module* configure-runtime #f
            (current-read-interaction rash-read-and-line-parse))
          (rash-line-parse ((open-input-string "") default-output-port-transformer 'string-port)
                           parg ...)))]))
