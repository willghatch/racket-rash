#lang racket/base

(provide my-grep)

(require "../../pipeline.rkt")
(require racket/string)
(require racket/function)
(require racket/format)
(require racket/runtime-path)
(require racket/port)

(define-runtime-path subprocess-pipeline.rkt "../subprocess-pipeline.rkt")


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
  (require racket/exn)

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
                                         #:err stdout-redirect)
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


   (define p+ep-out-str  "p+ep stdout")
   (define p+ep-err-str  "p+ep stderr")
   (define (p+ep [flag #f])
     (printf p+ep-out-str)
     (flush-output (current-output-port))
     (eprintf p+ep-err-str)
     (flush-output (current-error-port))
     (port->string (current-input-port))
     (if (equal? flag 'error)
         (error 'p+ep)
         (void))
     )

   (check-exn
    (λ (e)
      (and
       (string-contains?
        (exn->string e)
        p+ep-err-str)
       (not (string-contains?
             (exn->string e)
             (string-append p+ep-err-str p+ep-err-str)))))
    (λ ()
      (run-subprocess-pipeline/out
       (pipeline-member-spec `(,p+ep) #:err string-port-redirect)
       (pipeline-member-spec `(,p+ep error) #:err string-port-redirect)
       )))

   (check-exn
    (λ (e)
      (and
       (string-contains?
        (exn->string e)
        p+ep-err-str)
       (string-contains?
        (exn->string e)
        (string-append p+ep-err-str p+ep-err-str))))
    (λ ()
      (run-subprocess-pipeline/out
       (pipeline-member-spec `(,p+ep) #:err shared-string-port-redirect)
       (pipeline-member-spec `(,p+ep error) #:err shared-string-port-redirect)
       )))



  )


(module+ non-sandboxed-test
  ;; Here can go tests that rely on external programs,
  ;; but these tests must be run manually.

  (require rackunit)


  #|
  This is a stupid test -- I should write some proper tests that use
  some sort of test directory they can muck about in.  But for now, this
  one tests a file that I know exists.
  |#
  (check-pred
   number?
   (string->number
    (string-trim
     (run-pipeline/out `(cat ,subprocess-pipeline.rkt)
                       '(grep define)
                       `(,my-grep pipeline)
                       '(wc -l)))))

  )
