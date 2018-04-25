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

 =map=
 =filter=
 =foldl=
 )

(provide lsl)
(provide ls)
(provide =unix-with-xargs-behavior=)


(require
 "../pipeline-macro.rkt"
 racket/stream
 racket/string
 file/glob
 racket/stxparam
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
         #'(object-pipeline-member-spec (λ (prev-ret)
                                          (for-macro ([for-iter prev-ret])
                                                     (narg ...))))]
        [(#f narg ...)
         #'(object-pipeline-member-spec (λ (prev-ret)
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
             #'(object-pipeline-member-spec (λ (prev-ret)
                                              (for/list ([for-iter prev-ret])
                                                (run-pipeline
                                                 &out out-transformer
                                                 =basic-unix-pipe=
                                                 kwarg ...
                                                 narg ...))))]
            [(#f narg ...)
             #'(object-pipeline-member-spec (λ (prev-ret)
                                              (for/list ([for-iter prev-ret])
                                                (run-pipeline
                                                 &out out-transformer
                                                 =basic-unix-pipe=
                                                 kwarg ...
                                                 narg ...
                                                 for-iter))))]))))]))
(define-pipeline-operator =for/list/unix-input=
  #:joint
  (syntax-parser
    [(_ arg ...+)
     #'(object-pipeline-member-spec (λ (prev-ret)
                                      (for/list ([for-iter prev-ret])
                                        (run-pipeline
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


(require "../private/define-pipeline-alias.rkt")
(define-pipeline-alias lsl
  (syntax-parser [(_ arg ...)
                  #'(=quoting-basic-unix-pipe= 'ls '-l '--color=auto arg ...)]))
(define-simple-pipeline-alias ls 'ls '--color=auto)


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
                       (object-pipeline-member-spec
                        (λ (in)
                          (begin (set! pipe-arg (if (input-port? in) (port->string in) in))
                                 "")))
                       (u-pipeline-member-spec
                        (list (u-alias-func
                               (λ () (flatten (list 'cmd narg ...)))))))))]))]))

(define-pipeline-operator =map=
  #:joint (syntax-parser
            [(_ arg ...)
             (expand-pipeline-arguments
              #'(arg ...)
              #'iter-arg
              (syntax-parser
                [(#t narg ...)
                 #'(object-pipeline-member-spec (λ (prev-ret)
                                                  (map (λ (iter-arg) (narg ...))
                                                       prev-ret)))]
                [(#f narg ...)
                 #'(object-pipeline-member-spec (λ (prev-ret)
                                                  (map (λ (iter-arg) (narg ... iter-arg))
                                                       prev-ret)))]))]))

(define-pipeline-operator =filter=
  #:joint (syntax-parser
            [(_ arg ...)
             (expand-pipeline-arguments
              #'(arg ...)
              #'iter-arg
              (syntax-parser
                [(#t narg ...)
                 #'(object-pipeline-member-spec (λ (prev-ret)
                                                  (filter (λ (iter-arg) (narg ...))
                                                          prev-ret)))]
                [(#f narg ...)
                 #'(object-pipeline-member-spec (λ (prev-ret)
                                                  (filter (λ (iter-arg) (narg ... iter-arg))
                                                          prev-ret)))]))]))
(define-pipeline-operator =foldl=
  #:joint (syntax-parser
            [(_ accum-name accum-expr arg ...+)
             #'(object-pipeline-member-spec
                (λ (prev-ret)
                  (foldl (λ (iter-arg accum-name)
                           (syntax-parameterize ([current-pipeline-argument
                                                  (make-rename-transformer #'iter-arg)])
                             (arg ...)))
                         accum-expr
                         prev-ret)))]))

