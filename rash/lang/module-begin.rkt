#lang racket/base

(provide
 (except-out (all-from-out racket/base) #%module-begin)
 (rename-out [module-begin #%module-begin])
 (all-from-out rash)
 )

(require rash
         (submod "../private/lang-funcs.rkt" for-module-begin)
         racket/splicing
         (for-syntax racket/base
                     racket/stxparam-exptime
                     syntax/parse
                     syntax/strip-context))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(m-b arg ...)
     (let* ([implicit-key (gensym 'rash-implicit-starter-key-mb-)]
            [set (hash-set! implicit-pipe-starter-hash
                            implicit-key
                            (hash-ref implicit-pipe-starter-hash
                                      {syntax-parameter-value #'implicit-pipe-starter-key}))])
       (with-syntax ([(parg ...) (replace-context #'foo #'(arg ...))])
         ;; #%plain-module-begin would suppress printing...
         #`(#%module-begin
            (module* configure-runtime #f
              (current-read-interaction rash-read-and-line-parse))
            (splicing-syntax-parameterize ([implicit-pipe-starter-key
                                            (quote #,(datum->syntax #'here implicit-key))])
              (rash-line-parse ((open-input-string "") default-output-port-transformer 'string-port)
                               parg ...))
            )))]))
