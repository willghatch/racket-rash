#lang racket/base

(provide submit-predicate)

(require linea/read)

(define (submit-predicate interactive-text-port only-whitespace?)
  ;; If it can read until getting eof, it's good
  (with-handlers ([(λ _ #t) (λ _ #f)])
    (letrec ([read-rec (λ ()
                         (let ([result (linea-read-syntax 'interaction
                                                          interactive-text-port)])
                           (if (eof-object? result)
                               #t
                               (read-rec))))])
      (read-rec))))
