#lang racket/base

(provide
 (for-syntax
  prop:rash-pipeline-starter rash-pipeline-starter? rash-pipeline-starter-ref
  prop:rash-pipeline-joiner rash-pipeline-joiner? rash-pipeline-joiner-ref)
 define-rash-pipe
 ;; temporarily
 rash-pipeline-splitter
 the-implicit-pipe-starter
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

;; TODO - define for real
(define-rash-pipe the-implicit-pipe-starter/default
  (syntax-parser
    [(_ arg ...)
     #'(eprintf "implicit-pipe-starter used with args ~a~n" '(arg ...))])
  (λ (stx) (error 'the-implicit-pipe-starter/default "Can't be used as a pipeline joiner.  Also, that shouldn't normally be possible."))
  (λ (stx)
    (error 'the-implicit-pipe-starter/default "Can't be used as a normal macro."))
  )

#|
TODO - how do I expose this?  Not as a stxparam.
1:  Potentially in splitting lines is the rash macro I can use
    a macro that sets this with splicing-syntax-parameterize
    that gets the rest of the rash segment.  This would work well
    in modules, but not interactively.
2:  Interactively I could have some interactive variable that
    is set by this that determines the parameter when at top-level.
    So maybe I can detect when I'm in the top level of interactivity?
    That sounds difficult and brittle.
3:  If all macros into rash-land parameterize this, then the default
    can be a special value that looks up the current interactive default.
    The setting macro can check whether the current value is the flag value,
    and if so, set the interactive default that is looked up.
    This interactive-default would have to be namespace-specific.
    To accomplish that... I can have a magic hopefull-unique-name that I set
    with rash macros if it is unset in the namespace...
|#
(define-syntax-parameter the-implicit-pipe-starter
  #'the-implicit-pipe-starter/default)


(define-syntax (rash-pipeline-splitter stx)
  (syntax-parse stx #:literals (#'the-implicit-pipe-starter)
    [(_ (~and starter
              (~or (~var starter-explicit pipe-starter-op)
                   (the-implicit-pipe-starter)))
        args:not-pipeline-op ...
        rest ...)
     #'(rash-pipeline-splitter/rest ([starter args ...]) (rest ...))]
    [(rps iargs:not-pipeline-op ...+ rest ...)
     #'(rps (the-implicit-pipe-starter) iargs ... rest ...)]
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
  (syntax-parse stx #:literals (#'the-implicit-pipe-starter)
    [(_ op:pipe-starter-op arg:not-pipeline-op ...)
     (let ([slv (syntax-local-value #'op)])
       ({rash-pipeline-starter-ref slv} slv #'(op arg ...)))]
    [(rtss (the-implicit-pipe-starter) arg:not-pipeline-op ...)
     #`(rtss #,(syntax-parameter-value #'the-implicit-pipe-starter) arg ...)]))

(define-syntax (rash-transform-joiner-segment stx)
  (syntax-parse stx
    [(_ op:pipe-joiner-op arg:not-pipeline-op ...)
     (let ([slv (syntax-local-value #'op)])
       ({rash-pipeline-joiner-ref slv} slv #'(op arg ...)))]))

;; TODO - implement for real
(define (rash-do-transformed-pipeline . args)
  (eprintf "rash-do-transformed got: ~a~n" args))
