#lang racket/base

(provide
 current-repl-display-startup-hints?
 current-repl-startup-hints
 repl-display-startup-hint
 )
(require racket/random)

(define current-repl-display-startup-hints?
  (make-parameter #t))
(define current-repl-startup-hints
  (make-parameter #f))

(define (hint x)
  (printf "\n~a\n(To turn these hints off, run (current-repl-display-startup-hints? #f) in a rashrc file.)\n" x))

(define (repl-display-startup-hint)
  (when (current-repl-display-startup-hints?)
    (let ([hints (current-repl-startup-hints)])
      (if (sequence? hints)
          (hint (random-ref hints))
          (let ([hint-db (dynamic-require 'rash/private/repl-startup-hint-db
                                          'hints)])
            (current-repl-startup-hints hint-db)
            (hint (random-ref hint-db)))))))
