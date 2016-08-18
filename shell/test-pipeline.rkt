#lang racket/base

(require "pipeline.rkt")
(require racket/string)
(require racket/function)
(require racket/format)
(require racket/runtime-path)

(define-runtime-path pipeline.rkt "pipeline.rkt")


(define (grep-func str regex)
  (let ([r (cond [(regexp? regex) regex]
                 [(~a regex)])])
    (string-append
     (string-join (filter identity
                          (for/list ([line (string-split str "\n")])
                            (and (regexp-match r line) line)))
                  "\n")
     "\n")))
(define my-grep (shellify grep-func))

(module+ test
  ;; Only tests that only rely on functions should go here -- calling
  ;; external programs will fail in the test environment.
  (require rackunit)

  (check-equal? (run-pipeline/out '(echo hello))
                "hello\n" )
  (check-equal? (run-pipeline/out '(echo hello "\n" how are you? "\n" I am fine)
                                  `(,my-grep hello))
                "hello \n")
  (shell-alias 'aoeu '(echo aoeu))
  (check-equal? (run-pipeline/out '(aoeu hello))
                "aoeu hello\n" )

  )

(module+ main
  ;; Here can go tests that rely on external programs,
  ;; but these tests must be run manually.

  (require rackunit)


  ;; TODO - how to I wrap this so I get a nice summary as with raco test and the test module?
  (printf "If this exits without saying things passed, then things failed.~n")

  (check-equal? (string->number
                 (string-trim
                  (run-pipeline/out `(cat ,pipeline.rkt)
                                    '(grep define)
                                    `(,my-grep current-shell-functions)
                                    '(wc -l))))
                1)

  (printf "If it didn't say anything about falures, then the tests passed.~n")

  )
