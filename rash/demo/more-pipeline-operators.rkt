#lang racket/base


(provide
 =fors=
 =for/list=
 =for/stream=
 =for/list/unix-arg=
 =for/list/unix-input=

 =globbing-basic-unix-pipe=

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
