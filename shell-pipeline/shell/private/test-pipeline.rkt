#lang racket/base

(provide my-grep)

(require "../pipeline.rkt")
(require racket/string)
(require racket/function)
(require racket/format)
(require racket/runtime-path)
(require racket/port)

(define-runtime-path pipeline.rkt "../pipeline.rkt")


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

(define (my-grep2 regex)
  (for ([line (port->lines (current-input-port))])
    (when (regexp-match regex line)
      (displayln line))))

(define my-echo (λ args (displayln (string-join (map ~a args) " "))))

(module+ test
  ;; Only tests that only rely on functions should go here -- calling
  ;; external programs will fail in the test environment.
  (require rackunit)

  (check-equal? (run-pipeline/out `(,my-echo hello))
                "hello\n" )
  (check-equal? (run-pipeline/out `(,my-echo hello "\n" how are you? "\n" I am fine)
                                  `(,my-grep hello))
                "hello \n")


  ;; run-pipeline/out should raise an exception if it has a nonzero exit for the last member
  (check-exn exn?
             (λ () (run-pipeline/out (list (λ () (error 'test-case "exceptional!"))))))

  ;; No error if the last pipeline member is fine
  (check-equal? "hello\n"
                (run-pipeline/out #:strictness 'permissive
                                  (list (λ () (error 'test-case "exceptional!")))
                                  `(,my-echo hello)))
  (check-exn exn?
             (λ () (run-pipeline/out #:strictness 'strict
                                     (list (λ () (error 'test-case "exceptional!")))
                                     `(,my-echo hello))))

  ;; check stdout flag for functions
  (check-regexp-match "^my-test-func: exceptional!"
                      (run-pipeline/out #:strictness 'permissive
                                        (pipeline-member-spec
                                         (list (λ () (error 'my-test-func "exceptional!")))
                                         #:err 'stdout)
                                        (list (λ ()
                                                (display (port->string (current-input-port)))
                                                0))))
  (check-equal? "hello\n"
                (run-pipeline/out #:strictness 'permissive
                                  (list (λ () (error 'test-case "exceptional!")))
                                  `(,my-echo hello)))

  (check-equal? "hello\n"
                (run-pipeline/out #:strictness 'lazy
                                  #:lazy-timeout 0.1
                                  (list (λ ()
                                          (sleep 0.2)
                                          (error 'test-case "exceptional!")))
                                  `(,my-echo hello)))

  (check-exn exn?
             (λ ()
               (run-pipeline/out #:strictness 'strict
                                 #:lazy-timeout 0.1
                                 (list (λ ()
                                         (sleep 0.2)
                                         (error 'test-case "exceptional!")))
                                 `(,my-echo hello))))

  (check-pred pipeline?
              (run-pipeline #:background? #t
                            #:in #f
                            #:out #f
                            #:err #f
                            `(,my-echo hello)))
  (check-equal?
   (with-output-to-string
     (λ () (and/success (run-pipeline `(,my-echo hello))
                        (run-pipeline `(,my-echo hello)))))
   "hello\nhello\n")
  (check-equal?
   (with-output-to-string
     (λ () (or/success (run-pipeline `(,my-echo hello))
                       (run-pipeline `(,my-echo hello)))))
   "hello\n")
  (parameterize ([current-error-port (open-output-nowhere)])
    (check-equal?
     (with-output-to-string
       (λ () (and/success (run-pipeline `(,(λ () (error 'foobar "aoeu"))))
                          (run-pipeline `(,my-echo hello)))))
     "")
    (check-equal?
     (with-output-to-string
       (λ () (or/success (run-pipeline `(,(λ () (error 'foobar "aoeu"))))
                         (run-pipeline `(,my-echo hello)))))
     "hello\n"))

  (check-equal?
   (run-pipeline/out `(,(alias-func (λ _ `(,my-echo hellooooo)))
                       one two three four))
   "hellooooo\n")

  )


(module+ main
  ;; Here can go tests that rely on external programs,
  ;; but these tests must be run manually.

  (require rackunit)

  ;; TODO - how to I wrap this so I get a nice summary as with raco test and the test module?
  (printf "If this exits without saying things passed, then things failed.\n")

  (check-equal? (string->number
                 (string-trim
                  (run-pipeline/out `(cat ,pipeline.rkt)
                                    '(grep define)
                                    `(,my-grep shellify)
                                    '(wc -l))))
                1)

  (printf "If it didn't say anything about falures, then the tests passed.\n")

  )
