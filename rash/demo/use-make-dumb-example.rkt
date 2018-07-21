#!/usr/bin/env racket
#lang rash/demo/make

quux.txt : (list "bar.txt" (string-append "qwerty" ".txt")) {
  echo the dependencies of quux.txt are (current-dependencies) &>! quux.txt
}

qwerty.txt : {
  echo this file depends on nothing... &>! (current-target)
}

(define aoeu 'aoeu.txt)

foo.txt $aoeu : {
  echo hello &>! (current-target)
  ;echo this is going to error
  ;(error "I'm purposely erroring here")
}

bar.txt : foo.txt aoeu.txt $aoeu {
  ;; Note that current-dependencies includes the duplicate aoeu.txt
  echo current deps are (current-dependencies) &>! (current-target)
}

