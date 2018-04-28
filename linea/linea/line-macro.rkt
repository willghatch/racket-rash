#lang racket/base

(provide
 define-line-macro
 with-default-line-macro
 splicing-with-default-line-macro

 ;; TODO - this can be set with the with-default-line-macro forms.
 ;;        Should I export the bare syntax parameter as well?
 default-line-macro
 )
(require "private/line-macro-definitions.rkt")
