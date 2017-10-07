#lang racket/base

(require rash/private/lang-funcs)
(provide (all-from-out rash/private/lang-funcs))

(make-rash-reader-submodule rash/demo/rc17-demo-modbeg)
