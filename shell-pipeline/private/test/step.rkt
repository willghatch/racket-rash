#lang rash

(module+ test
  (require rackunit)
  
  (run-pipeline =basic-object-pipe/expression= "hello" =bind= greeting)
  (check-equal? (run-pipeline =basic-object-pipe/expression= (string-append greeting " world"))
                "hello world")
  
  (check-equal?
   (run-pipeline =basic-object-pipe/expression= "hello" =bind= greeting
                 =basic-object-pipe/expression= (string-append greeting " world"))
   "hello world"))



