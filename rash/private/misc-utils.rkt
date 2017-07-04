#lang racket/base

(provide (all-defined-out))

(require racket/dict)

(define (opref table key default)
  ;; For getting options with default out of a
  ;; parses-keyword-options result hash.
  (with-handlers ([(λ _ #t) (λ (e) default)])
    (cadr (dict-ref table key))))
