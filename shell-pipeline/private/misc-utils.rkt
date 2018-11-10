#lang racket/base

(provide (all-defined-out))

(require
 racket/dict
 racket/port
 "mostly-structs.rkt"
 )

(define (opref table key default)
  ;; For getting options with default out of a
  ;; parses-keyword-options result hash.
  (with-handlers ([(λ _ #t) (λ (e) default)])
    (cadr (dict-ref table key))))

(define (has-glob-characters? str)
  ;; Detect in a literal string segment whether there are glob characters
  ;; TODO - what is the full list of characters that should induce globbing?
  (regexp-match #px"\\*|\\?|\\{|\\}" str))

(define (open-output-spec spec)
  (cond [(file-redirection-spec? spec)
         (open-output-file (file-redirection-spec-file spec)
                           (file-redirection-spec-exists-flag spec))]
        [(equal? spec 'null) (open-output-nowhere)]
        [(path-string-symbol? spec)
         (open-output-file
          (path-string-sym->path spec)
          #:exists 'error)]
        [(list? spec)
         (open-output-file
          (path-string-sym->path (car spec))
          #:exists (cadr spec))]
        [else spec]))

(define (path-string-sym->path pss)
  (cond [(symbol? pss) (string->path (symbol->string pss))]
        [(string? pss) (string->path pss)]
        [else pss]))
