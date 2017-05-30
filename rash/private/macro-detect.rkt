#lang racket/base

(provide
 (for-syntax
  prop:rash-pipeline-starter rash-pipeline-starter? rash-pipeline-starter-ref
  prop:rash-pipeline-joiner rash-pipeline-joiner? rash-pipeline-joiner-ref)
 define-rash-pipe
 ;; TODO - if the macro to change the starter is in a stop-list of local-expand,
 ;; it can cause the default to be wrong because it works via side-effect!
 ;; How can this be fixed?
 ;; Maybe instead of letting it run as a macro, the outer rash macro can detect
 ;; it and eagerly do all side-effect-y things.  This would also require eagerly
 ;; splitting pipelines.
 default-pipe-starter!
 ;; temporarily
 rash-pipeline-splitter
 =basic-object-pipe=
 =crappy-basic-unix-pipe=
 )

(require
 racket/stxparam
 shell/mixed-pipeline
 racket/port
 (for-syntax
  racket/base
  syntax/parse
  racket/stxparam-exptime
  ))

(begin-for-syntax
  (define-values (prop:rash-pipeline-starter
                  rash-pipeline-starter?
                  rash-pipeline-starter-ref)
    (make-struct-type-property 'rash-pipeline-starter))
  (define-values (prop:rash-pipeline-joiner
                  rash-pipeline-joiner?
                  rash-pipeline-joiner-ref)
    (make-struct-type-property 'rash-pipeline-joiner))

  (struct rash-pipeline-operator
    (as-starter as-joiner outside-rash-macro)
    #:property prop:rash-pipeline-starter (λ (inst . args)
                                            (apply
                                             {rash-pipeline-operator-as-starter inst}
                                             args))
    #:property prop:rash-pipeline-joiner (λ (inst . args)
                                            (apply
                                             {rash-pipeline-operator-as-joiner inst}
                                             args))
    #:property prop:procedure (struct-field-index outside-rash-macro))


  (define-syntax-class pipe-starter-op
    (pattern op:id
             #:when (rash-pipeline-starter? (syntax-local-value #'op (λ () #f)))))
  (define-syntax-class pipe-joiner-op
    (pattern op:id
             #:when (rash-pipeline-joiner? (syntax-local-value #'op (λ () #f)))))
  (define-syntax-class not-pipeline-op
    (pattern (~and (~not x:pipe-joiner-op)
                   (~not x:pipe-starter-op))))
  )

(define-syntax (define-rash-pipe/no-kw stx)
  (syntax-parse stx
    [(def name as-starter as-joiner outside-of-rash)
     #'(define-syntax name
         (rash-pipeline-operator
          as-starter
          as-joiner
          outside-of-rash))]))

(define-syntax (define-rash-pipe stx)
  (syntax-parse stx
    [(def name
       (~or (~optional (~seq #:start s-impl:expr))
            (~optional (~seq #:joint j-impl:expr))
            (~optional (~seq #:macro m-impl:expr)))
       ...)
     (with-syntax ([starter (if (attribute s-impl)
                                #'s-impl
                                #'(λ (stx)
                                    (raise-syntax-error
                                     #''name
                                     "Can't be used as a pipeline starter operator")))]
                   [joiner (if (attribute j-impl)
                               #'j-impl
                               #'(λ (stx)
                                   (raise-syntax-error
                                    #''name
                                    "Can't be used as a pipeline joint operator")))]
                   [nmacro (if (attribute m-impl)
                               #'m-impl
                               #'(λ (stx)
                                   (raise-syntax-error
                                    #''name
                                    "Can't be used as a normal macro")))])
       #'(define-rash-pipe/no-kw name starter joiner nmacro))]))

;; TODO - make a way to define a rash-pipe-operator by desugaring to existing operators (or a combination of operators)

;; TODO - define for real
(define-rash-pipe default-pipe-starter
  #:start
  (λ (stx) (raise-syntax-error 'default-pipe-starter "No default pipe starter has been set, and the default default is to be an error.")))

(define-rash-pipe =crappy-basic-unix-pipe=
  #:start
  (syntax-parser
    [(_ arg ...+) #'(u-pipeline-member-spec '(arg ...) (current-error-port))])
  #:joint
  (syntax-parser
    [(_ arg ...+) #'(u-pipeline-member-spec '(arg ...) (current-error-port))]))

;; TODO - the basic object pipes should still recognize the prev-arg $_ parameter
(define-rash-pipe =basic-object-pipe=
  #:start
  (syntax-parser
    [(_ arg ...+) #'(obj-pipeline-member-spec (λ () (arg ...)))])
  #:joint
  (syntax-parser
    [(_ arg ...+)
     #'(obj-pipeline-member-spec (λ (prev-ret) (arg ... prev-ret)))]))

(define-rash-pipe =basic-object-pipe/left=
  #:start
  (syntax-parser
    [(_ arg ...+) #'(obj-pipeline-member-spec (λ () (arg ...)))])
  #:joint
  (syntax-parser
    [(_ func arg ...)
     #'(obj-pipeline-member-spec (λ (prev-ret) (func prev-ret arg ...)))]))

;; TODO - so much... name, $_ arg, etc
(provide =obj=)
(define-rash-pipe =obj=
  #:start
  (syntax-parser
    [(_ arg ...+) #'(obj-pipeline-member-spec (λ () (arg ...)))])
  #:joint
  (syntax-parser
    [(_ arg ...+)
     #'(composite-pipeline-member-spec
        (list (obj-pipeline-member-spec (λ (prev-ret) (if (input-port? prev-ret)
                                                          ;; TODO - should this trim?
                                                          (port->string prev-ret)
                                                          prev-ret)))
              (obj-pipeline-member-spec (λ (prev-ret) (arg ... prev-ret)))))]))

(begin-for-syntax
  (define top-level-pipe-starter-default #'default-pipe-starter))

(define-syntax-parameter get-implicit-pipe-starter
  (λ () top-level-pipe-starter-default))
(define-syntax-parameter set-implicit-pipe-starter
  (λ (new-setter)
    (set! top-level-pipe-starter-default new-setter)))


(define-syntax (default-pipe-starter! stx)
  (syntax-parse stx
    [(_ new-starter:pipe-starter-op)
     (begin
       ({syntax-parameter-value #'set-implicit-pipe-starter}
        #'new-starter)
       #'(void))]))


;; TODO - accept pipeline modifiers about bg, env, etc
(define-syntax (rash-pipeline-splitter stx)
  (syntax-parse stx
    [(_ starter:pipe-starter-op args:not-pipeline-op ... rest ...)
     #'(rash-pipeline-splitter/rest ([starter args ...]) (rest ...))]
    [(rps iargs:not-pipeline-op ...+ rest ...)
     #`(rps #,({syntax-parameter-value #'get-implicit-pipe-starter})
            iargs ... rest ...)]
    ))

(define-syntax (rash-pipeline-splitter/rest stx)
  (syntax-parse stx
    [(rpsr (done-parts ...) ())
     #'(rash-pipeline-splitter/done done-parts ...)]
    [(rpsr (done-parts ...) (op:pipe-joiner-op arg:not-pipeline-op ... rest ...))
     #'(rpsr (done-parts ... [op arg ...]) (rest ...))]))

(define-syntax (rash-pipeline-splitter/done stx)
  (syntax-parse stx
    [(_ (starter startarg ...) (joiner joinarg ...) ...)
     #'(rash-do-transformed-pipeline
        (rash-transform-starter-segment starter startarg ...)
        (rash-transform-joiner-segment joiner joinarg ...) ...)]))

;; TODO - this should have args like #:bg, #:env, etc
(define-syntax (rash-do-pipeline stx)
  (syntax-parse stx
    [(_ startseg joinseg ...)
     #'(rash-do-transformed-pipeline (rash-transform-starter-segment startseg)
                                     (rash-transform-joiner-segment joinseg) ...)]))

(define-syntax (rash-transform-starter-segment stx)
  (syntax-parse stx
    [(_ op:pipe-starter-op arg:not-pipeline-op ...)
     (let ([slv (syntax-local-value #'op)])
       ({rash-pipeline-starter-ref slv} slv #'(op arg ...)))]))

(define-syntax (rash-transform-joiner-segment stx)
  (syntax-parse stx
    [(_ op:pipe-joiner-op arg:not-pipeline-op ...)
     (let ([slv (syntax-local-value #'op)])
       ({rash-pipeline-joiner-ref slv} slv #'(op arg ...)))]))

;; TODO - implement for real
(define (rash-do-transformed-pipeline #:bg [bg #f] . args)
  (run-pipeline args #:bg bg #:in (current-input-port) #:out (current-output-port)))
