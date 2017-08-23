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
 =obj-if-def/globbing-unix-if-undef=
 )


(require
 rash
 racket/stream
 racket/string
 file/glob
 shell/mixed-pipeline
 (for-syntax
  racket/base
  syntax/parse
  racket/string
  shell/private/filter-keyword-args
  ))



(pipeop =globbing-basic-unix-pipe=
        [(_ arg ...+)
         (let-values ([(kwargs pargs) (filter-keyword-args #'(arg ...))])
           #`(=basic-unix-pipe=
              #,@kwargs
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
                      pargs)))])

(pipeop =obj-if-def/unix-if-undef=
        [(_ cmd arg ...)
         (if (and (identifier? #'cmd) (identifier-binding #'cmd))
             #'(=object-pipe= cmd arg ...)
             #'(=quoting-basic-unix-pipe= cmd arg ...))])

(pipeop =obj-if-def/globbing-unix-if-undef=
        [(_ cmd arg ...)
         (if (and (identifier? #'cmd) (identifier-binding #'cmd))
             #'(=object-pipe= cmd arg ...)
             #'(=globbing-basic-unix-pipe= cmd arg ...))])

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

(define out-transformer (λ (o) (string-trim (port->string o))))

(define-pipeline-operator =for/list/unix-arg=
  #:joint
  (syntax-parser
    [(_ arg ...+)
     (let-values ([(kwargs pargs) (filter-keyword-args #'(arg ...))])
       (with-syntax ([(kwarg ...) (datum->syntax #f kwargs)]
                     [(parg ...) (datum->syntax #f pargs)])
         (expand-pipeline-arguments
          #'((quote-if-id-not-current-arg parg) ...)
          #'for-iter
          (syntax-parser
            [(#t narg ...)
             #'(obj-pipeline-member-spec (λ (prev-ret)
                                           (for/list ([for-iter prev-ret])
                                             (rash-run-pipeline
                                              &out out-transformer
                                              =basic-unix-pipe=
                                              kwarg ...
                                              narg ...))))]
            [(#f narg ...)
             #'(obj-pipeline-member-spec (λ (prev-ret)
                                           (for/list ([for-iter prev-ret])
                                             (rash-run-pipeline
                                              &out out-transformer
                                              =basic-unix-pipe=
                                              kwarg ...
                                              narg ...
                                              for-iter))))]))))]))
(define-pipeline-operator =for/list/unix-input=
  #:joint
  (syntax-parser
    [(_ arg ...+)
     #'(obj-pipeline-member-spec (λ (prev-ret)
                                   (for/list ([for-iter prev-ret])
                                     (rash-run-pipeline
                                      &in (open-input-string (format "~a" for-iter))
                                      &out out-transformer
                                      =basic-unix-pipe=
                                      (quote-if-id-not-current-arg arg) ...))))]))


#|
;; I don't want to add another package dependency, but here is another fine pipe operator.
(require data/monad)
(provide >>=)
(pipeop >>= [(_ f) #'(=object-pipe= chain f current-pipeline-argument)])
|#


(require (for-syntax "rash-alias.rkt"))
(require "define-rash-alias.rkt")
(provide =aliasing-unix-pipe=)
(pipeop =aliasing-unix-pipe=
        [(_ cmd:rash-alias-id arg ...)
         (let ([slv (syntax-local-value #'cmd)])
           ({rash-alias-ref slv} slv #'(cmd arg ...)))]
        [(_ arg ...)
         #'(=quoting-basic-unix-pipe= arg ...)])
(provide lsl)
(define-rash-alias lsl
  (syntax-parser [(_ arg ...)
                  #'(=quoting-basic-unix-pipe= 'ls '-l '--color=auto arg ...)]))
(provide ls)
(define-simple-rash-alias ls 'ls '--color=auto)


(provide =unix-with-xargs-behavior=)
(require racket/port racket/list)
#|
This does various different things and needs to be simplified.  First of all
it needs to do the alias checking itself rather than depending on =aliasing-unix-pipe=,
then it needs to standardize the output...
|#
(define-pipeline-operator =unix-with-xargs-behavior=
  #:start (syntax-parser [(_ arg ...+) #'(=aliasing-unix-pipe= arg ...)])
  #:joint (syntax-parser
            [(_ cmd arg ...)
             (expand-pipeline-arguments
              (map (syntax-parser
                     [(~literal current-pipeline-argument)
                      #'current-pipeline-argument]
                     [x:id #'(quote x)]
                     [e #'e])
                   (syntax->list #'(arg ...)))
              #'pipe-arg
              (syntax-parser
                [(#f narg ...) #'(=aliasing-unix-pipe= cmd narg ...)]
                [(#t narg ...)
                 #'(let ([pipe-arg #f])
                     (composite-pipeline-member-spec
                      (list
                       (obj-pipeline-member-spec
                        (λ (in)
                          (begin (set! pipe-arg (if (input-port? in) (port->string in) in))
                                 "")))
                       (u-pipeline-member-spec
                        (list (u-alias-func
                               (λ () (flatten (list 'cmd narg ...)))))))))]))]))
