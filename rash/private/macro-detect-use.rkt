#lang racket/base

(require
 "macro-detect.rkt"
 racket/stxparam
 racket/port
 (for-syntax
  racket/base
  syntax/parse))

(define-rash-pipe =testpipe=
  #:start
  (syntax-parser [(_ arg ...) 
                  (printf "testpipe as start with ~a~n" #'(arg ...))
                  #'(printf "testpipe at runtime in start with args: ~a~n" (list arg ...))])
  #:joint
  (syntax-parser [(_ arg ...) 
                  (printf "testpipe as join with ~a~n" #'(arg ...))
                  #'(printf "testpipe at runtime with args: ~a~n" (list arg ...))])
  #:macro
  (syntax-parser [(_ arg ...) 
                  (printf "testpipe as normal with ~a~n" #'(arg ...))
                  #'(error 'testpipe "not a real impl~n")]))

(define-syntax tp-rename (make-rename-transformer #'=testpipe=))

(define-syntax (loc stx)
  (syntax-parse stx
    [(loc arg ...+)
     (datum->syntax stx (map (λ (s)
                               (local-expand s
                                             (syntax-local-context)
                                             (list #'default-pipe-starter!)))
                             (syntax->list #'(arg ...))))]))

;(rash-pipeline-splitter =testpipe= 1 2 3 =testpipe= 4 5 6)
;(let ()
#;(loc
  (rash-pipeline-splitter  =testpipe= 1 2 3 =testpipe= 4 5 6)
  (rash-pipeline-splitter  =testpipe= 1 2 3 tp-rename 'after-rename 5 6)
  (rash-pipeline-splitter 1 2 3 =testpipe= 4 5 6)
  (default-pipe-starter! tp-rename)
  (rash-pipeline-splitter 'after-default-switch 2 3 =testpipe= 4 5 6)
#;(syntax-parameterize ([the-implicit-pipe-starter #'=testpipe=])
  (rash-pipeline-splitter  97 1 2 3 =testpipe= 4 5 6))
)
;)

(define-syntax =o= (make-rename-transformer #'=basic-object-pipe=))
(define-syntax =u= (make-rename-transformer #'=crappy-basic-unix-pipe=))

(rash-pipeline-splitter  =o= string-upcase "hello" =o= string-append "aeou")
(rash-pipeline-splitter  =u= ls -l =u= grep macro)
(rash-pipeline-splitter  =u= ls -l =u= grep macro =o= port->string =o= string-upcase =u= wc)
  ;(rash-pipeline-splitter  =testpipe= 1 2 3 tp-rename 'after-rename 5 6)
