#lang racket/base
(require "../main.rkt")
(require (submod "lang-funcs.rkt" for-module-begin))

(define-namespace-anchor ns-a)
(define repl-namespace (namespace-anchor->namespace ns-a))
(provide repl-namespace)
