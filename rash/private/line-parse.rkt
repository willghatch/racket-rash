#lang racket/base

(provide
 rash-read-and-line-parse
 )


(require
 syntax/parse
 linea/read
 (for-template
  linea/line-parse
  (only-in shell/private/pipeline-macro-parse rash-set-defaults)
  ))


(define (rash-read-and-line-parse src in)
  (let ([stx (linea-read-syntax src in)])
    (if (eof-object? stx)
        stx
        (syntax-parse stx
          [e #'(rash-set-defaults ((current-input-port)
                                   (current-output-port)
                                   (current-error-port))
                                  (linea-line-parse e))]))))

