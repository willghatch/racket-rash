#lang racket/base

(provide
 rash-read-and-line-parse
 )


(require
 (only-in shell/private/pipeline-macro-parse rash-set-defaults)
 linea/read
 syntax/parse
 racket/stxparam
 racket/splicing
 racket/string
 racket/port
 shell/mixed-pipeline
 (for-syntax
  racket/base
  syntax/parse
  racket/stxparam-exptime
  syntax/keyword
  racket/dict
  linea/read
  shell/private/misc-utils
  (for-syntax
   racket/base
   syntax/parse
   )))

(define (rash-read-and-line-parse src in)
  (let ([stx (linea-read-syntax src in)])
    (if (eof-object? stx)
        stx
        (syntax-parse stx
          [e #'(rash-set-defaults ((current-input-port)
                                   (current-output-port)
                                   (current-error-port))
                                  (linea-line-parse e))]))))

