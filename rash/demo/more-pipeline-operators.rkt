#lang racket/base

#|
These are essentially a bunch of proof-of-concept pipeline operators.
|#


(provide
 ;; for making for/whatever operators out of the various for/whatever forms
 =fors=
 =for/list=
 =for/stream=

 ;; use each member of the input list as an argument in a unix command
 =for/list/unix-arg=
 ;; use each member of the input list as the stdin to a unix command
 =for/list/unix-input=

 =globbing-basic-unix-pipe=

 ;; treat the pipeline as an object pipe if the command is bound in racket,
 ;; otherwise treat it as a unix command.
 =obj-if-def/unix-if-undef=
 )


(require
 rash
 racket/stream
 file/glob
 shell/mixed-pipeline
 (for-syntax
  racket/base
  syntax/parse
  racket/string
  ))



(pipeop =globbing-basic-unix-pipe=
        [(_ arg ...+)
         #`(=basic-unix-pipe=
            #,@(map (λ (s) (syntax-parse s
                             [(~and x (~or xi:id xs:str))
                              (cond [(regexp-match #px"\\*|\\{|\\}|\\?"
                                                   (format "~a" (syntax->datum #'x)))
                                     #`(glob #,(datum->syntax
                                                #'x
                                                (format "~a" (syntax->datum #'x))
                                                #'x #'x))]
                                    [(string-prefix? (format "~a" (syntax->datum #'x))
                                                     "~")
                                     #'(with-handlers ([(λ _ #t) (λ _ 'x)])
                                         (format "~a" (expand-user-path
                                                       (format "~a" 'x))))]
                                    [else #'(quote x)])]
                             [e #'e]))
                    (syntax->list #'(arg ...))))])

(pipeop =obj-if-def/unix-if-undef=
        [(_ cmd arg ...)
         (if (and (identifier? #'cmd) (identifier-binding #'cmd))
             #'(=object-pipe= cmd arg ...)
             #'(=quoting-basic-unix-pipe= cmd arg ...))])

(define-pipeline-operator =fors=
  #:joint
  (syntax-parser
    [( _ for-macro arg ...+)
     (expand-pipeline-arguments
      #'(arg ...)
      #'for-iter
      (syntax-parser
        [(#t narg ...)
         #'(obj-pipeline-member-spec (λ (prev-ret)
                                       (for-macro ([for-iter prev-ret])
                                                  (narg ...))))]
        [(#f narg ...)
         #'(obj-pipeline-member-spec (λ (prev-ret)
                                       (for-macro ([for-iter prev-ret])
                                                  (narg ... for-iter))))]))]))

(pipeop =for/list= [(_ arg ...+) #'(=fors= for/list arg ...)])
(pipeop =for/stream= [(_ arg ...+) #'(=fors= for/list arg ...)])

(define-syntax (quote-if-id-not-current-arg stx)
  (syntax-parse stx
    #:literals (current-pipeline-argument)
    [(_ current-pipeline-argument) #'current-pipeline-argument]
    [(_ x:id) #'(quote x)]
    [(_ e) #'e]))

(define-pipeline-operator =for/list/unix-arg=
  #:joint
  (syntax-parser
    [(_ arg ...+)
     (expand-pipeline-arguments
      #'((quote-if-id-not-current-arg arg) ...)
      #'for-iter
      (syntax-parser
        [(#t narg ...)
         #'(obj-pipeline-member-spec (λ (prev-ret)
                                       (for/list ([for-iter prev-ret])
                                         (rash-do-pipeline
                                          (=basic-unix-pipe=
                                           narg ...)))))]
        [(#f narg ...)
         #'(obj-pipeline-member-spec (λ (prev-ret)
                                       (for/list ([for-iter prev-ret])
                                         (rash-do-pipeline
                                          (=basic-unix-pipe=
                                           narg ...
                                           for-iter)))))]))]))
(define-pipeline-operator =for/list/unix-input=
  #:joint
  (syntax-parser
    [(_ arg ...+)
     #'(obj-pipeline-member-spec (λ (prev-ret)
                                   (for/list ([for-iter prev-ret])
                                     (rash-do-pipeline
                                      #:in (open-input-string (format "~a" for-iter))
                                      (=basic-unix-pipe=
                                       (quote-if-id-not-current-arg arg) ...)))))]))


#|
;; I don't want to add another package dependency, but here is another fine pipe operator.
(require data/monad)
(provide >>=)
(pipeop >>= [(_ f) #'(=object-pipe= chain f current-pipeline-argument)])
|#

