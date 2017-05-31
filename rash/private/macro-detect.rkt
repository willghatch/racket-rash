#lang racket/base

(provide
 (for-syntax
  prop:rash-pipeline-starter rash-pipeline-starter? rash-pipeline-starter-ref
  prop:rash-pipeline-joiner rash-pipeline-joiner? rash-pipeline-joiner-ref
  expand-pipeline-arguments
  )
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

 current-rash-pipeline-argument

 =composite-pipe=

 =basic-object-pipe=
 =basic-object-pipe/left=
 =basic-object-pipe/expression=
 =object-pipe=
 =object-pipe/left=
 =object-pipe/expression=

 =non-quoting-basic-unix-pipe=
 =crappy-basic-unix-pipe=
 )

(require
 racket/stxparam
 shell/mixed-pipeline
 racket/port
 (for-syntax
  racket/base
  racket/match
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
       #'(define-rash-pipe/no-kw name starter joiner nmacro))]))


;; TODO - define for real
(define-rash-pipe default-pipe-starter
  #:start
  (λ (stx) (raise-syntax-error 'default-pipe-starter "No default pipe starter has been set, and the default default is to be an error.")))

(define-rash-pipe =composite-pipe=
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

(define-rash-pipe =non-quoting-basic-unix-pipe=
  #:start
  (syntax-parser
    [(_ arg ...+) #'(u-pipeline-member-spec (list arg ...) (current-error-port))])
  #:joint
  (syntax-parser
    [(_ arg ...+) #'(u-pipeline-member-spec (list arg ...) (current-error-port))]))

(define-rash-pipe =crappy-basic-unix-pipe=
  #:start
  (syntax-parser
    [(_ arg ...+) #'(u-pipeline-member-spec '(arg ...) (current-error-port))])
  #:joint
  (syntax-parser
    [(_ arg ...+) #'(u-pipeline-member-spec '(arg ...) (current-error-port))]))


(begin-for-syntax
  (define (stx-replace stx from-id to-id)
    ;; Replace from-id with to-id recursively.
    ;; If the result is eq? to stx, then no change was made.
    ;; If the result is NOT eq? to stx, then at least one change was made.

    #|
    syntax-e can produce:
    symbol
    syntax-pair
    empty-list
    immutable vector of syntax objects
    immutable box of syntax objects
    TODO - immutable hash table with syntax object values (but not necessarily syntax object keys!)
    TODO - immutable prefab struct with syntax objects
    some other datum like number, string, etc
    |#
    (define (rec s)
      (stx-replace s from-id to-id))
    (define (->s datum)
      (datum->syntax stx datum stx stx))
    (if (and (identifier? stx) (free-identifier=? stx from-id))
        to-id
        (let ([expanded (if (syntax? stx)
                            (syntax-e stx)
                            stx)])
          (match expanded
            [(cons l r) (let ([l* (rec l)]
                              [r* (rec r)])
                          (if (and (eq? l l*)
                                   (eq? r r*))
                              stx
                              (if (syntax? stx)
                                  (->s (cons l* r*))
                                  (cons l* r*))))]
            [(vector elems ...) (let ([elems* (map rec elems)])
                                  (if (andmap (λ(x)x)
                                              (map eq? elems* elems))
                                      stx
                                      (->s apply vector-immutable elems*)))]
            [(box x) (let ([x* (rec x)])
                       (if (eq? x x*)
                           stx
                           (->s (box-immutable x))))]
            [_ stx]))))
  )

(define-syntax-parameter current-rash-pipeline-argument #f)
;; placeholder value so local-expand doesn't barf
(define rash-pipeline-argument-standin #f)

(begin-for-syntax
  (define (expand-pipeline-arguments
           stx
           ;; arg-replacement will replace current-rash-pipeline-argument
           arg-replacement
           ;; transformer should be a syntax parser, and the first element
           ;; of syntax will be #t if at least one replacement was made,
           ;; else false.
           transformer)
    (syntax-parse stx
      [(arg ...+)
       (with-syntax ([prev-arg #'rash-pipeline-argument-standin])
         (local-expand #'(let ([prev-arg #f]) 5) 'expression '())
         (let ([e-args
                (map (λ (s) (local-expand
                             #`(syntax-parameterize
                                   ([current-rash-pipeline-argument
                                     (syntax-id-rules () [_ prev-arg])])
                                 #,s)
                             'expression '()))
                     (syntax->list #'(arg ...)))])
           ;; Detect if #'prev-arg is in the expanded syntax, and replace it.
           ;; If it's not in the expanded syntax, then put the argument at the end.
           (define new-e-args (stx-replace e-args
                                           #'prev-arg
                                           arg-replacement))
           (define explicit-reference-exists? (not (eq? new-e-args e-args)))
           (transformer #`(#,(datum->syntax #'here explicit-reference-exists?)
                           #,@new-e-args))))]))
  )

(define-rash-pipe =basic-object-pipe=
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
(define-rash-pipe =basic-object-pipe/left=
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

(define-rash-pipe =basic-object-pipe/expression=
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

(define-rash-pipe =object-pipe=
  #:start (syntax-parser [(_ arg ...+) #'(=basic-object-pipe= arg ...)])
  #:joint
  (syntax-parser [(_ arg ...+) (with-port-sugar #'(=basic-object-pipe= arg ...))]))
(define-rash-pipe =object-pipe/left=
  #:start (syntax-parser [(_ arg ...+) #'(=basic-object-pipe/left= arg ...)])
  #:joint
  (syntax-parser [(_ arg ...+) (with-port-sugar #'(=basic-object-pipe/left= arg ...))]))
(define-rash-pipe =object-pipe/expression=
  #:start (syntax-parser [(_ e) #'(=basic-object-pipe/expression= e)])
  #:joint
  (syntax-parser [(_ e) (with-port-sugar #'(=basic-object-pipe/expression= e))]))


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
    [(_ arg ...)
     #'(rash-pipeline-splitter/start rash-pipeline-splitter/done/do arg ...)]))

(define-syntax (rash-pipeline-splitter/start stx)
  (syntax-parse stx
    [(_ done-macro starter:pipe-starter-op args:not-pipeline-op ... rest ...)
     #'(rash-pipeline-splitter/rest done-macro ([starter args ...]) (rest ...))]
    [(rps done-macro iargs:not-pipeline-op ...+ rest ...)
     #`(rps done-macro
            #,({syntax-parameter-value #'get-implicit-pipe-starter})
            iargs ... rest ...)]))

(define-syntax (rash-pipeline-splitter/rest stx)
  (syntax-parse stx
    [(rpsr done-macro (done-parts ...) ())
     #'(done-macro done-parts ...)]
    [(rpsr done-macro
           (done-parts ...)
           (op:pipe-joiner-op arg:not-pipeline-op ... rest ...))
     #'(rpsr done-macro (done-parts ... [op arg ...]) (rest ...))]))

(define-syntax (rash-pipeline-splitter/done/do stx)
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
    [(tr op:pipe-starter-op arg:not-pipeline-op ...)
     (let* ([slv (syntax-local-value #'op)]
            [transformed ({rash-pipeline-starter-ref slv} slv #'(op arg ...))])
       (syntax-parse transformed
         ;; If the transformed result is another pipeline operator, try again
         [(op:pipe-starter-op arg:not-pipeline-op ...)
          #'(tr op arg ...)]
         [_ transformed]))]))

(define-syntax (rash-transform-joiner-segment stx)
  (syntax-parse stx
    [(tr op:pipe-joiner-op arg:not-pipeline-op ...)
     (let* ([slv (syntax-local-value #'op)]
            [transformed ({rash-pipeline-joiner-ref slv} slv #'(op arg ...))])
       (syntax-parse transformed
         ;; If the transformed result is another pipeline operator, try again
         [(op:pipe-joiner-op arg:not-pipeline-op ...)
          #'(tr op arg ...)]
         [_ transformed]))]))

;; TODO - implement for real
(define (rash-do-transformed-pipeline #:bg [bg #f] . args)
  (run-pipeline args #:bg bg #:in (current-input-port) #:out (current-output-port)))
