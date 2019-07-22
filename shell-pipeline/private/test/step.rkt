#lang linea "step-lang.rkt"
(require rackunit
         rash
         linea/defaults)


;; TODO - this errors if put inside a test submodule
(rash-block
   (run-pipeline =basic-object-pipe/expression= "hello" =bind= greeting)
   (check-equal? greeting "hello"))
