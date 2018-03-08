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

 =composite-pipe=

 =basic-object-pipe=
 =object-pipe=

 =basic-object-pipe/expression=
 =object-pipe/expression=

 =basic-object-pipe/form=
 =object-pipe/form=

; =basic-object-pipe/left=
; =object-pipe/left=

 =basic-unix-pipe=
 =quoting-basic-unix-pipe=
 )

(require
 racket/stxparam
 racket/port
 racket/list
 racket/match
 racket/string
 shell/mixed-pipeline
 "pipeline-operator-transform.rkt"
 "mostly-structs.rkt"
 (for-syntax
  racket/base
  syntax/parse
  syntax/keyword
  "pipeline-operator-detect.rkt"
  "misc-utils.rkt"
  "filter-keyword-args.rkt"
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
       (with-syntax ([(e-arg ...)
                      (map (λ (s) (syntax-parse s
                                    [(~or (form-arg ...) x:id)
                                     (local-expand
                                      #`(syntax-parameterize
                                            ([current-pipeline-argument
                                              (make-set!-transformer
                                               (λ (id)
                                                 (syntax-case id ()
                                                   [_ #'prev-arg])))])
                                          #,s)
                                      'expression '())]
                                    [else s]))
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

;; operator for receiving first-class segments (IE possibly-composite member specs)
(define-pipeline-operator =pipeline-segment=
  ;; TODO -- this should behave the same as a starter or joint
  #:joint
  (syntax-parser
    [(_ segment ...)
     #'(composite-pipeline-member-spec (list segment ...))]))

;;;; object pipes

(define-pipeline-operator =basic-object-pipe=
  #:start
  (syntax-parser
    [(_ arg ...+) #'(object-pipeline-member-spec (λ () (arg ...)))])
  #:joint
  (syntax-parser
    [(_ arg ...+)
     (expand-pipeline-arguments
      #'(arg ...)
      #'prev-ret
      (λ (expanded-stx)
        (syntax-parse expanded-stx
          [(#t narg ...)
           #'(object-pipeline-member-spec (λ (prev-ret) (narg ...)))]
          [(#f narg ...)
           #'(object-pipeline-member-spec (λ (prev-ret) (arg ... prev-ret)))])))]))
#;(define-pipeline-operator =basic-object-pipe/left=
  #:start
  (syntax-parser
    [(_ arg ...+) #'(object-pipeline-member-spec (λ () (arg ...)))])
  #:joint
  (syntax-parser
    [(_ arg ...+)
     (expand-pipeline-arguments
      #'(arg ...)
      #'prev-ret
      (λ (expanded-stx)
        (syntax-parse expanded-stx
          [(#t narg ...)
           #'(object-pipeline-member-spec (λ (prev-ret) (narg ...)))]
          [(#f narg ...)
           #'(object-pipeline-member-spec (λ (prev-ret) (prev-ret arg ...)))])))]))

;; Pipe for just a single expression that isn't considered pre-wrapped in parens.
(define-pipeline-operator =basic-object-pipe/expression=
  #:start
  (syntax-parser
    [(_ e) #'(object-pipeline-member-spec (λ () e))])
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
           #'(object-pipeline-member-spec (λ (prev-ret) ne))])))]))

;; Like =basic-object-pipe=, but doesn't local-expand each argument separately.
(define-pipeline-operator =basic-object-pipe/form=
  #:start
  (syntax-parser
    [(_ arg ...+) #'(object-pipeline-member-spec (λ () (arg ...)))])
  #:joint
  (syntax-parser
    [(_ arg ...+)
     (expand-pipeline-arguments
      #'((arg ...))
      #'prev-ret
      (λ (expanded-stx)
        (syntax-parse expanded-stx
          [(_ (narg ...))
           #'(object-pipeline-member-spec (λ (prev-ret) (narg ...)))])))]))


(define-for-syntax (with-port-sugar pipe-stx)
  #`(=composite-pipe= (=basic-object-pipe= (λ (x) (if (input-port? x)
                                                      (port->string x)
                                                      x)))
                      #,pipe-stx))

(define-pipeline-operator =object-pipe=
  #:start (syntax-parser [(_ arg ...+) #'(=basic-object-pipe= arg ...)])
  #:joint
  (syntax-parser [(_ arg ...+) (with-port-sugar #'(=basic-object-pipe= arg ...))]))
#;(define-pipeline-operator =object-pipe/left=
  #:start (syntax-parser [(_ arg ...+) #'(=basic-object-pipe/left= arg ...)])
  #:joint
  (syntax-parser [(_ arg ...+) (with-port-sugar #'(=basic-object-pipe/left= arg ...))]))
(define-pipeline-operator =object-pipe/expression=
  #:start (syntax-parser [(_ e) #'(=basic-object-pipe/expression= e)])
  #:joint
  (syntax-parser [(_ e) (with-port-sugar #'(=basic-object-pipe/expression= e))]))
(define-pipeline-operator =object-pipe/form=
  #:start (syntax-parser [(_ e) #'(=basic-object-pipe/form= e)])
  #:joint
  (syntax-parser [(_ e) (with-port-sugar #'(=basic-object-pipe/form= e))]))

;;;; unix-y pipes

(define (apply-output-transformer transformer out-port)
  (match transformer
    ['port out-port]
    ['string (port->string out-port)]
    ['trim (string-trim (port->string out-port))]
    ['lines (string-split (port->string out-port) "\n")]
    ['words (string-split (port->string out-port))]
    [tx (if (procedure? tx)
            ;; TODO - if this doesn't read the whole port there could be problems
            (tx out-port)
            (error 'apply-output-transformer
                   (format "Neither a procedure nor a known transformer name: ~a"
                           tx)))]))



(define-for-syntax unix-pipe-option-table
  (list (list '#:as check-expression)
        (list '#:e> check-expression)
        (list '#:e>! check-expression)
        (list '#:e>> check-expression)
        (list '#:err check-expression)
        (list '#:env check-expression)
        ;; IE success predicate -- is returning 1 an error?
        (list '#:success check-expression)
        ))

(define-for-syntax (basic-unix-pipe/ordered-args stx)
  (syntax-parse stx
    [(arg-maybe-opt ...+)
     (define-values (opts rest-stx)
       (parse-keyword-options #'(arg-maybe-opt ...)
                              unix-pipe-option-table
                              #:no-duplicates? #t
                              #:incompatible '((#:e> #:e>! #:e>> #:err))))
     (syntax-parse rest-stx
       [(arg ...)
        (let ([success-pred (opref opts '#:success #'(pipeline-default-option))]
              ;; TODO - hook up env
              [env-extend (opref opts '#:env #''())]
              [err (cond [(opref opts '#:e> #f)
                          => (syntax-parser [out #'(list (quote out) 'error)])]
                         [(opref opts '#:e>> #f)
                          => (syntax-parser [out #'(list (quote out) 'append)])]
                         [(opref opts '#:e>! #f)
                          => (syntax-parser [out #'(list (quote out) 'truncate)])]
                         [(opref opts '#:err #f)]
                         [else #'(pipeline-default-option)])]
              [as (opref opts '#:as #f)])
          (if as
              #`(composite-pipeline-member-spec
                 (list
                  (unix-pipeline-member-spec (flatten (list arg ...))
                                          #:err #,err
                                          #:success #,success-pred)
                  (object-pipeline-member-spec (λ (out-port)
                                                 (apply-output-transformer #,as out-port)))))
              #`(unix-pipeline-member-spec (flatten (list arg ...))
                                        #:err #,err
                                        #:success #,success-pred)))])]))

(define-for-syntax (basic-unix-pipe stx)
  (syntax-parse stx
    [(_ arg ...+)
     (let-values ([(kwargs pargs) (filter-keyword-args #'(arg ...))])
       (basic-unix-pipe/ordered-args
        (datum->syntax #f (append kwargs pargs))))]))

(define-pipeline-operator =basic-unix-pipe=
  #:start basic-unix-pipe
  #:joint basic-unix-pipe)

(pipeop =quoting-basic-unix-pipe=
        [(_ arg ...+)
         (let-values ([(kwargs pargs) (filter-keyword-args #'(arg ...))])
           #`(=basic-unix-pipe=
              #,@kwargs
              #,@(map (λ (s) (syntax-parse s
                               [x:id #'(quote x)]
                               [e #'e]))
                      pargs)))])
