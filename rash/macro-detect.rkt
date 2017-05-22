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
 )

(require
 racket/stxparam
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

(define-syntax (define-rash-pipe stx)
  (syntax-parse stx
    [(def name as-starter as-joiner outside-of-rash)
     #'(define-syntax name
         (rash-pipeline-operator
          as-starter
          as-joiner
          outside-of-rash))]))

;; TODO - make a way to define a rash-pipe-operator by desugaring to existing operators (or a combination of operators)

;; TODO - define for real
(define-rash-pipe default-pipe-starter
  (syntax-parser
    [(_ arg ...)
     #'(eprintf "default-pipe-starter used with args ~a~n" '(arg ...))])
  (λ (stx) (error 'default-pipe-starter "Can't be used as a pipeline joiner.  Also, that shouldn't normally be possible."))
  (λ (stx)
    (error 'default-pipe-starter "Can't be used as a normal macro."))
  )

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
(define (rash-do-transformed-pipeline . args)
  (eprintf "rash-do-transformed got: ~a~n" args))
