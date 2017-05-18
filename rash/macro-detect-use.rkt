#lang racket/base

(require
 "macro-detect.rkt"
 racket/stxparam
 (for-syntax
  racket/base
  syntax/parse))

(define-rash-pipe =testpipe=
  (syntax-parser [(_ arg ...) 
                  (printf "testpipe as start with ~a~n" #'(arg ...))
                  #'(printf "testpipe at runtime in start with args: ~a~n" (list arg ...))])
  (syntax-parser [(_ arg ...) 
                  (printf "testpipe as join with ~a~n" #'(arg ...))
                  #'(printf "testpipe at runtime with args: ~a~n" (list arg ...))])
  (syntax-parser [(_ arg ...) 
                  (printf "testpipe as normal with ~a~n" #'(arg ...))
                  #'(error 'testpipe "not a real impl~n")]))

(define-syntax tp-rename (make-rename-transformer #'=testpipe=))


;(rash-pipeline-splitter =testpipe= 1 2 3 =testpipe= 4 5 6)
(rash-pipeline-splitter  =testpipe= 1 2 3 =testpipe= 4 5 6)
(rash-pipeline-splitter  =testpipe= 1 2 3 tp-rename 'after-rename 5 6)
(rash-pipeline-splitter 1 2 3 =testpipe= 4 5 6)
(syntax-parameterize ([the-implicit-pipe-starter #'=testpipe=])
  (rash-pipeline-splitter  97 1 2 3 =testpipe= 4 5 6))
