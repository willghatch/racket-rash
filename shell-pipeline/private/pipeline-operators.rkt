#lang racket/base

(provide
 ;; TODO - names of these two pipe definers
 define-pipeline-operator

 #|
 Maybe defpipeop?  defop?  Or should this even exist?
 My original idea with this was to have a quick and easy way to define new operators
 even on the fly in an interactive repl, but maybe you don't care to define new
 operators while in an interactive session.  But then again... I might want to.
 But you need to be comfortable enough defining macros and understanding rash
 to use it...
 |#
 pipeop

 ;; TODO - what should be in the basic set exported by this file?
 ;;        Should I make this mostly just how to define them and
 ;;        a bare-bones one for each of unix, object, and composite?

 current-pipeline-argument

 (for-syntax expand-pipeline-arguments)

 =bind=
 =composite-pipe=
 =pipeline-segment=

 =basic-object-pipe=
 =object-pipe=

 =basic-object-pipe/expression=
 =object-pipe/expression=

 =basic-object-pipe/form=
 =object-pipe/form=

 =basic-unix-pipe=
 )

(require
 racket/stxparam
 racket/port
 racket/list
 racket/match
 racket/string
 shell/mixed-pipeline
 "mostly-structs.rkt"
 (submod "subprocess-pipeline.rkt" resolve-command-path)
 "pipeline-operator-generics.rkt"
 (for-syntax
  racket/base
  syntax/parse
  syntax/keyword
  "misc-utils.rkt"
  "filter-keyword-args.rkt"
  racket/stxparam-exptime
  racket/match
  ))

;;;;;;;;; Defining forms


(define-syntax (define-pipeline-operator stx)
  (syntax-parse stx
    [(def name
       (~or (~optional (~seq #:start s-impl:expr))
            (~optional (~seq #:joint j-impl:expr))
            (~optional (~seq #:operator o-impl:expr))
            ;; I think it's probably best to NOT allow it to be used as a normal macro.
            ;(~optional (~seq #:macro m-impl:expr))
            )
       ...)
     (when (or (and (attribute o-impl)
                    (attribute j-impl))
               (and (attribute o-impl)
                    (attribute s-impl)))
       (raise-syntax-error 'define-pipeline-operator
                           "#:operator can't be used with #:start or #:joint"
                           stx))
     (with-syntax ([starter (or (and (attribute s-impl) #'s-impl)
                                (and (attribute o-impl) #'o-impl)
                                #'(λ (stx)
                                    (raise-syntax-error
                                     (syntax->datum #'name)
                                     "Can't be used as a pipeline starter operator"
                                     stx)))]
                   [joint (or (and (attribute j-impl) #'j-impl)
                              (and (attribute o-impl) #'o-impl)
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
       (if (attribute o-impl)
           #'(begin
               (define-for-syntax parsername o-impl)
               (define-pipeline-operator/no-kw name parsername parsername nmacro))
           #'(define-pipeline-operator/no-kw name starter joint nmacro)))]))

(define-syntax (pipeop stx)
  (syntax-parse stx
    [(defpipe name:id clause ...+)
     #'(begin
         (define-for-syntax parsername (syntax-parser clause ...))
         (define-pipeline-operator name #:operator parsername))]))

;;;;;;;;;;;;;;;; Pipeline argument detection, replacement functions

(define-syntax-parameter current-pipeline-argument
  (λ (stx) (raise-syntax-error 'current-pipeline-argument
                               "Can't use implicit pipeline argument here."
                               stx)))

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
(define expand-pipeline-arguments-standin #f)
(define-for-syntax (expand-pipeline-arguments
                    stx
                    ;; transformer should be a syntax parser, and the first element
                    ;; of syntax will be #t if at least one replacement was made,
                    ;; else false.
                    ;; The rest of the elements of the syntax will be transformed
                    ;; versions of the arg fields of stx, except they will be
                    ;; turned into functions that must be called with the value
                    ;; expected for current-pipeline-argument.
                    transformer)
  (syntax-parse stx
    [(arg ...+)
     (with-syntax ([(e-arg ...)
                    (map (λ (s) (syntax-parse s
                                  [(~or (form-arg ...) x:id)
                                   (local-expand
                                    #`(λ (prev-arg)
                                        (syntax-parameterize
                                            ([current-pipeline-argument
                                              (make-set!-transformer
                                               (λ (id)
                                                 (syntax-case id ()
                                                   [_ (quote-syntax
                                                       (begin
                                                         expand-pipeline-arguments-standin
                                                         prev-arg))])))])
                                          #,s))
                                    'expression '())]
                                  [else #`(λ (prev-arg) #,s)]))
                         (syntax->list #'(arg ...)))])
       (with-syntax ([explicit-ref-exists?
                      (datum->syntax #'here
                                     (stx-contains-id?
                                      #'(e-arg ...)
                                      (quote-syntax
                                       expand-pipeline-arguments-standin)))])
         (transformer #'(explicit-ref-exists?
                         e-arg ...))))]))





;;;; object pipes

(define-pipeline-operator =basic-object-pipe=
  #:operator
  (syntax-parser
    [(_ arg ...+)
     (expand-pipeline-arguments
      #'(arg ...)
      (λ (expanded-stx)
        (syntax-parse expanded-stx
          [(#t narg ...)
           #'(=pipeline-segment=
              (object-pipeline-member-spec (λ (prev-ret) ((narg prev-ret) ...))))]
          [(#f narg ...)
           #'(=pipeline-segment=
              (object-pipeline-member-spec (λ ([prev-ret (pipeline-default-option)])
                                             (if (pipeline-default-option? prev-ret)
                                                 ((narg prev-ret) ...)
                                                 ((narg prev-ret) ... prev-ret)))))])))]))


;; Like =basic-object-pipe=, but doesn't local-expand each argument separately.
(define-pipeline-operator =basic-object-pipe/form=
  #:start
  (syntax-parser
    [(_ arg ...+) #'(=pipeline-segment=
                     (object-pipeline-member-spec (λ () (arg ...))))])
  #:joint
  (syntax-parser
    [(_ arg ...+)
     #'(=pipeline-segment=
        (object-pipeline-member-spec
         (λ (prev-ret)
           (syntax-parameterize ([current-pipeline-argument
                                  (make-rename-transformer #'prev-ret)])
             (arg ...)))))]))

(define (port-sugar-transformer x)
  (if (input-port? x)
      (begin0 (port->string x)
        (close-input-port x))
      x))

(define-for-syntax (with-port-sugar pipe-stx)
  #`(=composite-pipe= (=basic-object-pipe= port-sugar-transformer)
                      #,pipe-stx))

(define-pipeline-operator =object-pipe=
  #:start (syntax-parser [(_ arg ...+) #'(=basic-object-pipe= arg ...)])
  #:joint
  (syntax-parser [(_ arg ...+) (with-port-sugar #'(=basic-object-pipe= arg ...))]))
(define-pipeline-operator =object-pipe/expression=
  #:start (syntax-parser [(_ e) #'(=basic-object-pipe/expression= e)])
  #:joint
  (syntax-parser [(_ e) (with-port-sugar #'(=basic-object-pipe/expression= e))]))
(define-pipeline-operator =object-pipe/form=
  #:start (syntax-parser [(_ e) #'(=basic-object-pipe/form= e)])
  #:joint
  (syntax-parser [(_ e) (with-port-sugar #'(=basic-object-pipe/form= e))]))


