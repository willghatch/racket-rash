#lang racket/base

(provide
 (except-out (all-from-out racket/base) #%module-begin)
 (rename-out [rash-module-begin #%module-begin])
 (all-from-out rash)
 )

(require
 rash
 (submod "../private/lang-funcs.rkt" for-module-begin)
 )
