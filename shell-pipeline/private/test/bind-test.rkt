#lang racket

(require rash)

(module+ test
  (require rackunit syntax/macro-testing)
  (require racket/port)

  ;; Sanity check that functions in place of subprocesses are supported with =unix-pipe=

  ;; First check that having =bind= doesn't foul everything up.
  (check-equal?
   (run-pipeline =unix-pipe= (λ () (display "Hi"))
                 =basic-object-pipe= port->string
                 =bind= im-name
                 =basic-object-pipe= (λ (x) (string-append "bye" (string-upcase x))))
   "byeHI")


  ;; Then check that the variable is actually bound.
  (check-equal?
   (run-pipeline =unix-pipe= (λ () (display "My name is Inigo Montoya"))
                 =basic-object-pipe= port->string
                 =bind= im-name
                 =basic-object-pipe/expression= im-name)
   "My name is Inigo Montoya")

  (check-equal?
     (run-pipeline =unix-pipe= (λ () (display "My name is Inigo Montoya"))
                   =basic-object-pipe= port->string
                   =bind= im-name
                   =basic-object-pipe= (λ (x) (string-append im-name (string-upcase x))))
     "My name is Inigo MontoyaMY NAME IS INIGO MONTOYA")

  ; Can't enforce this right now because of racket/contract's syntax-local-lift-expression
  ; bug; having =basic-object-pipe/expression= completely local-expand its Racket syntax
  ; would trigger eager lifts in the first pass of module expansion such that
  ; (module ... (let () (pipeline ...)) (pipeline)) would break.
  #;(check-exn exn:fail:syntax?
             (λ ()
               (convert-compile-time-error
                ;; a-name should not be visible before it is bound.
                (run-pipeline =basic-object-pipe/expression= a-name
                              =basic-object-pipe/expression= 5
                              =bind= a-name))
               ))


  ; Make sure it work across lines.
  (run-pipeline =basic-object-pipe/expression= "hello" =bind= greeting)
  (check-equal? (run-pipeline =basic-object-pipe/expression= (string-append greeting " world"))
                "hello world")

  (let ()
    (run-pipeline =basic-object-pipe/expression= "hello" =bind= greeting)
    (check-equal? (run-pipeline =basic-object-pipe/expression= (string-append greeting " =bind="))
                  "hello =bind="))

  )
