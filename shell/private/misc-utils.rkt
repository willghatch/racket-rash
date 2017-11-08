#lang racket/base

(provide (all-defined-out))

(require racket/dict)

(define (opref table key default)
  ;; For getting options with default out of a
  ;; parses-keyword-options result hash.
  (with-handlers ([(λ _ #t) (λ (e) default)])
    (cadr (dict-ref table key))))

(define (has-glob-characters? str)
  ;; Detect in a literal string segment whether there are glob characters
  ;; TODO - what is the full list of characters that should induce globbing?
  (regexp-match #px"\\*|\\?|\\{|\\}" str))
