#lang racket/base

(provide
 ;; TODO - names of these two pipe definers
 define-pipeline-operator
 pipeop

 current-pipeline-argument

 (for-syntax expand-pipeline-arguments)

 =composite-pipe=

 =basic-object-pipe=
 =basic-object-pipe/left=
 =basic-object-pipe/expression=
 =object-pipe=
 =object-pipe/left=
 =object-pipe/expression=

 =for/list=

 =non-quoting-basic-unix-pipe=
 =crappy-basic-unix-pipe=
 )

(require
 racket/stxparam
 racket/port
 shell/mixed-pipeline
 "pipeline-operator-transform.rkt"
 (for-syntax
  racket/base
  syntax/parse
  "pipeline-operator-detect.rkt"
  racket/stxparam-exptime
  racket/match
  ))

;;;;;;;;; Defining forms

(define-syntax (define-pipeline-operator/no-kw stx)
  (syntax-parse stx
    [(def name as-starter as-joiner outside-of-rash)
     #'(define-syntax name
         (rash-pipeline-operator
          as-starter
          as-joiner
          outside-of-rash))]))

(define-syntax (define-pipeline-operator stx)
  (syntax-parse stx
    [(def name
       (~or (~optional (~seq #:start s-impl:expr))
            (~optional (~seq #:joint j-impl:expr))
            ;; I think it's probably best to NOT allow it to be used as a normal macro.
            ;(~optional (~seq #:macro m-impl:expr))
            )
       ...)
     (with-syntax ([starter (if (attribute s-impl)
                                #'s-impl
                                #'(λ (stx)
                                    (raise-syntax-error
                                    (syntax->datum #'name)
                                    "Can't be used as a pipeline starter operator"
                                    stx)))]
                   [joiner (if (attribute j-impl)
                               #'j-impl
                               #'(λ (stx)
                                   (raise-syntax-error
                                    (syntax->datum #'name)
                                    "Can't be used as a pipeline joint operator"
                                    stx)))]
                   [nmacro (if #f #;(attribute m-impl)
                               #'m-impl
                               #'(λ (stx)
                                   (raise-syntax-error
                                    (syntax->datum #'name)
                                    "Must be used as a rash pipeline operator"
                                    stx)))])
       #'(define-pipeline-operator/no-kw name starter joiner nmacro))]))

(define-syntax (pipeop stx)
  (syntax-parse stx
    [(defpipe name:id clause ...+)
     (with-syntax ([parsername (datum->syntax
                                stx
                                (gensym (string-append
                                         (symbol->string (syntax->datum #'name))
                                         "-parser-")))])
       #'(begin
           (define-for-syntax parsername (syntax-parser clause ...))
           (define-pipeline-operator name #:start parsername #:joint parsername)))]))

;;;;;;;;;;;;;;;; Pipeline argument detection, replacement functions

(define-syntax-parameter current-pipeline-argument #f)

(define-for-syntax (stx-contains-id? stx id)
  ;; Does the syntax contain id somethere?
  (define (rec s)
    (stx-contains-id? s id))
  (if (and (identifier? stx) (free-identifier=? stx id))
      #t
      (let ([expanded (if (syntax? stx)
                          (syntax-e stx)
                          stx)])
        (match expanded
          [(cons l r) (or (rec l) (rec r))]
          [(vector elems ...) (ormap rec elems)]
          [(box x) (rec x)]
          [(hash-table (key val) ...) (or (ormap rec val)
                                          (ormap rec key))]
          [(? struct?) (ormap rec
                              (vector->list (struct->vector expanded)))]
          [_ #f]))))

#|
TODO - this function should imperatively set a flag saying whether the
pipeline argument was explicitly used, because a macro could make it
disappear (and I don't want to then come back as something
re-appended).
|#
(define-for-syntax (expand-pipeline-arguments
                    stx
                    ;; arg-replacement will replace current-pipeline-argument
                    arg-replacement
                    ;; transformer should be a syntax parser, and the first element
                    ;; of syntax will be #t if at least one replacement was made,
                    ;; else false.
                    transformer)
  (syntax-parse stx
    [(arg ...+)
     (with-syntax ([prev-arg arg-replacement])
       (with-syntax ([(e-arg ...) (map (λ (s) (local-expand
                                          #`(syntax-parameterize
                                                ([current-pipeline-argument
                                                  (make-set!-transformer
                                                   (λ (id)
                                                     (syntax-case id ()
                                                       [_ #'prev-arg])))])
                                              #,s)
                                          'expression '()))
                                  (syntax->list #'(arg ...)))])
         (with-syntax ([explicit-ref-exists?
                        (datum->syntax #'here
                                       (stx-contains-id? #'(e-arg ...)
                                                         arg-replacement))])
           (transformer #'(explicit-ref-exists?
                           e-arg ...)))))]))




;;;;;;;;;;;;;;;;;;;;;;;;;;; Basic pipe operators

(define-pipeline-operator =composite-pipe=
  #:start
  (syntax-parser
    [(_ (start-op:pipe-starter-op start-arg:not-pipeline-op ...)
        (join-op:pipe-joiner-op join-arg:not-pipeline-op ...) ...)
     #'(composite-pipeline-member-spec
        (list (rash-transform-starter-segment start-op start-arg ...)
              (rash-transform-joiner-segment join-op join-arg ...) ...))])
  #:joint
  (syntax-parser
    [(_ (op:pipe-joiner-op arg:not-pipeline-op ...) ...+)
     #'(composite-pipeline-member-spec
        (list (rash-transform-joiner-segment op arg ...) ...))]))

;;;; object pipes

(define-pipeline-operator =basic-object-pipe=
  #:start
  (syntax-parser
    [(_ arg ...+) #'(obj-pipeline-member-spec (λ () (arg ...)))])
  #:joint
  (syntax-parser
    [(_ arg ...+)
     (expand-pipeline-arguments
        #'(arg ...)
        #'prev-ret
        (λ (expanded-stx)
          (syntax-parse expanded-stx
            [(#t narg ...)
             #'(obj-pipeline-member-spec (λ (prev-ret) (narg ...)))]
            [(#f narg ...)
             #'(obj-pipeline-member-spec (λ (prev-ret) (arg ... prev-ret)))])))]))
(define-pipeline-operator =basic-object-pipe/left=
  #:start
  (syntax-parser
    [(_ arg ...+) #'(obj-pipeline-member-spec (λ () (arg ...)))])
  #:joint
  (syntax-parser
    [(_ arg ...+)
     (expand-pipeline-arguments
        #'(arg ...)
        #'prev-ret
        (λ (expanded-stx)
          (syntax-parse expanded-stx
            [(#t narg ...)
             #'(obj-pipeline-member-spec (λ (prev-ret) (narg ...)))]
            [(#f narg ...)
             #'(obj-pipeline-member-spec (λ (prev-ret) (prev-ret arg ...)))])))]))

(define-pipeline-operator =basic-object-pipe/expression=
  #:start
  (syntax-parser
    [(_ e) #'(obj-pipeline-member-spec (λ () e))])
  #:joint
  (syntax-parser
    [(_ e)
     (expand-pipeline-arguments
        #'(e)
        #'prev-ret
        (λ (expanded-stx)
          (syntax-parse expanded-stx
            ;; Ignore the possibility of throwing away the pipe argument
            [(_ ne)
             #'(obj-pipeline-member-spec (λ (prev-ret) ne))])))]))


(define-for-syntax (with-port-sugar pipe-stx)
  #`(=composite-pipe= (=basic-object-pipe= (λ (x) (if (input-port? x)
                                                           (port->string x)
                                                           x)))
                           #,pipe-stx))

(define-pipeline-operator =object-pipe=
  #:start (syntax-parser [(_ arg ...+) #'(=basic-object-pipe= arg ...)])
  #:joint
  (syntax-parser [(_ arg ...+) (with-port-sugar #'(=basic-object-pipe= arg ...))]))
(define-pipeline-operator =object-pipe/left=
  #:start (syntax-parser [(_ arg ...+) #'(=basic-object-pipe/left= arg ...)])
  #:joint
  (syntax-parser [(_ arg ...+) (with-port-sugar #'(=basic-object-pipe/left= arg ...))]))
(define-pipeline-operator =object-pipe/expression=
  #:start (syntax-parser [(_ e) #'(=basic-object-pipe/expression= e)])
  #:joint
  (syntax-parser [(_ e) (with-port-sugar #'(=basic-object-pipe/expression= e))]))

(define-syntax (def-forpipe stx)
  (syntax-parse stx
    [(_ name for-stx)
     #'(define-pipeline-operator name
         #:joint
         (syntax-parser
           [(_ arg ...+)
            (expand-pipeline-arguments
             #'(arg (... ...))
             #'for-iter
             (syntax-parser
               [(#t narg (... ...))
                #'(obj-pipeline-member-spec (λ (prev-ret)
                                              (for-stx ([for-iter prev-ret])
                                                       (narg (... ...)))))]
               [(#f narg (... ...))
                #'(obj-pipeline-member-spec (λ (prev-ret)
                                              (for-stx ([for-iter prev-ret])
                                                       (narg (... ...) for-iter))))]))]))]))

(def-forpipe =for/list= for/list)
;(def-forpipe =for/stream= for/stream)


;;;; unix-y pipes

(define-pipeline-operator =non-quoting-basic-unix-pipe=
  #:start
  (syntax-parser
    [(_ arg ...+) #'(u-pipeline-member-spec (list arg ...))])
  #:joint
  (syntax-parser
    [(_ arg ...+) #'(u-pipeline-member-spec (list arg ...))]))

(define-pipeline-operator =crappy-basic-unix-pipe=
  #:start
  (syntax-parser
    [(_ arg ...+) #'(u-pipeline-member-spec '(arg ...))])
  #:joint
  (syntax-parser
    [(_ arg ...+) #'(u-pipeline-member-spec '(arg ...))]))

