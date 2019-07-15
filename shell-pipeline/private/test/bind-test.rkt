#lang racket/base

(require "../../pipeline-macro.rkt")

(require (for-syntax racket/base syntax/parse))
(define-syntax (debug stx)
  (syntax-parse stx
    [(_ arg)
     (printf "stx: ~a\n" (syntax-debug-info #'arg))
     #'arg]))

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

  
  (check-exn exn?
             (λ ()
               (convert-compile-time-error
                ;; a-name should not be visible before it is bound.
                (run-pipeline =basic-object-pipe/expression= a-name
                              =basic-object-pipe/expression= 5
                              =bind= a-name))
               ))

  #;(check-equal?
     (run-pipeline =unix-pipe= (λ () (printf "This is a test\nto be sure things\nare generally working."))
                   =unix-pipe= (letrec ([f (λ (regexp)
                                             (define l (read-line))
                                             (when (and (not (eof-object? l))
                                                        (regexp-match regexp l))
                                               (displayln l))
                                             (when (not (eof-object? l))
                                               (f regexp))
                                             )])
                                 f) #px"re"
                   =basic-object-pipe= port->string
                   =basic-object-pipe= string-upcase)
     "TO BE SURE THINGS\nARE GENERALLY WORKING.\n")

  )
