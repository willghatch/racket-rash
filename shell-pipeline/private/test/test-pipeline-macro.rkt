#lang racket/base

(require "../../pipeline-macro.rkt")

(module+ test
  (require rackunit)
  (require racket/port)

  ;; Sanity check that functions in place of subprocesses are supported with =unix-pipe=
  (check-equal?
   (run-pipeline =unix-pipe= (λ () (display "My name is Inigo Montoya"))
                 =basic-object-pipe= port->string)
   "My name is Inigo Montoya")

  (check-equal?
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
