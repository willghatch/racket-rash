#lang racket/base

(provide

 define-pipeline-operator/no-kw
 =composite-pipe=
 =pipeline-segment=
 =basic-object-pipe/expression=
 =basic-unix-pipe=
 =bind=

 ;transform-starter-segment
 ;transform-joint-segment

 (for-syntax

  pipeline-starter
  pipeline-joint
  not-pipeline-op

  dispatch-pipeline-starter
  dispatch-pipeline-joint

  core-pipeline-starter
  core-pipeline-joint
  pipeline-starter-macro
  pipeline-joint-macro


  ))

(require
 "basic-unix-pipe-helper-funcs.rkt"
 "mostly-structs.rkt"
 (for-syntax
  racket/base
  syntax/parse
  syntax-generic2
  ))

(begin-for-syntax
  (define-syntax-generic core-pipeline-starter)
  (define-syntax-generic core-pipeline-joint)
  (define-syntax-generic pipeline-starter-macro)
  (define-syntax-generic pipeline-joint-macro)

  #|
  ?? Does <syntax-generic>? return #t for an identifier bound to it or to an application form using it?

  (my-generic? #'id)
  (my-generic? #'(id arg ...))

  Both, apparently!  The predicate is supposed to match on the same
  things that the Racket macro expander matches on when detecting macro
  uses.  So it matches the form for identifier macros as well as for
  head-of-list macros.
  |#

  ;; For splitting macro to delimit pipeline segments
  (define-syntax-class (pipeline-starter def-ctx)
    (pattern op:id #:when (or (core-pipeline-starter? #'(op) def-ctx)
                              (pipeline-starter-macro? #'(op) def-ctx))))
  (define-syntax-class (pipeline-joint def-ctx)
    (pattern op:id #:when (or (core-pipeline-joint? #'(op) def-ctx)
                              (pipeline-joint-macro? #'(op) def-ctx))))
  (define-syntax-class (not-pipeline-op def-ctx)
    (pattern (~and (~not (~var x (pipeline-joint def-ctx)))
                   (~not (~var x (pipeline-starter def-ctx))))))




  (define (pipeline-starter->core stx)
    (cond
      [(core-pipeline-starter? stx) stx]
      [(pipeline-starter-macro? stx)
       (pipeline-starter->core
        (apply-as-transformer pipeline-starter-macro 'expression #f stx))]
      [else (error 'pipeline-starter->core "not a pipeline starter ~a\n" stx)]))
  (define (pipeline-joint->core stx)
    (cond
      [(core-pipeline-joint? stx) stx]
      [(pipeline-joint-macro? stx)
       (pipeline-joint->core
        (apply-as-transformer pipeline-joint-macro 'expression #f stx))]
      [else (error 'pipeline-joint->core "not a pipeline joint ~a\n" stx)]))

  #|
  For binding, I need the core desugaring thing to return the syntax to make a pipeline-member-spec, but also the names of any bindings (in both original and transformed form).  Then the next pipeline segment will need to be in a new context that can see that binding and has it bound to... maybe a transformer that will get a value out of a box?  And the spec form for that binding will need to run code that sets that box.  So I need to generate a getter and setter, and in the binding-spec-generator expression I need to let-syntax it to the setter, and further in the pipeline I need to let-syntax it to the getter.  Except I also want to be able to set! it later, so I need the getter to be a set!-transformer.
  Then the outer driver thing will have the original and transformed binding names, and can arrange all the let-syntax stuff as well as an outer definition of the given names if it is in a definition context.

  What arguments does it need to accept?  It needs the syntax object itself, obviously.  It needs a definition context, I guess?  Does it need a new one for each new expansion?  They should probably be parent/child definition contexts.

  (-> stx definition-context (values syntax (listof id)))
  |#
  (define (dispatch-pipeline-starter stx def-ctx)
    (define core-stx (pipeline-starter->core stx))
    (apply-as-transformer core-pipeline-starter 'expression #f core-stx def-ctx))
  (define (dispatch-pipeline-joint stx def-ctx)
    (define core-stx (pipeline-joint->core stx))
    (apply-as-transformer core-pipeline-joint 'expression #f core-stx def-ctx))
  )

;; basic definition form, wrapped by the better one in "pipeline-operators.rkt"
(define-syntax define-pipeline-operator/no-kw
  (syntax-parser [(_ name as-starter as-joint outside-of-rash)
                  #'(define-syntax name
                      (generics
                       [pipeline-starter-macro as-starter]
                       [pipeline-joint-macro as-joint]
                       ;; TODO - how to do this with syntax-generics?  It was in the paper, but I don't see it in the library.
                       ;[racket-macro outside-of-rash]
                       ))]))

(define-syntax (transform-starter-segment stx)
  (syntax-parse stx [(_ arg ...) (dispatch-pipeline-starter #'(arg ...))]))
(define-syntax (transform-joint-segment stx)
  (syntax-parse stx [(_ arg ...) (dispatch-pipeline-joint #'(arg ...))]))

(define-for-syntax (composite-pipe-helper segments def-ctx)
  (for/fold ([done-stx-list '()]
             [lifted-ids '()])
            ([segment segments])
    (let-values ([(done-stx ids) (dispatch-pipeline-joint segment def-ctx)])
      (values (append done-stx-list (list done-stx))
              (append lifted-ids ids)))))

(define-syntax =composite-pipe=
  (generics
   [core-pipeline-starter
    (λ (stx def-ctx)
      (syntax-parse stx
        [(_ ((~var start-op (pipeline-starter def-ctx))
             (~var start-arg (not-pipeline-op def-ctx)) ...)
            ((~var join-op (pipeline-joint def-ctx))
             (~var join-arg (not-pipeline-op def-ctx)) ...) ...)
         (define-values (stx1 ids1)
           (dispatch-pipeline-starter #'(start-op start-arg ...) def-ctx))
         (define-values (stxs2 ids2)
           (composite-pipe-helper (syntax->list #'((join-op join-arg ...) ...))
                                  def-ctx))
         (define stxs (cons stx1 stxs2))
         (define ids (append ids1 ids2))
         (values
          #`(composite-pipeline-member-spec (list #,@stxs))
          ids)]))]
   [core-pipeline-joint
    (λ (stx def-ctx)
      (syntax-parse stx
        [(_ ((~var op (pipeline-joint def-ctx))
             (~var arg (not-pipeline-op def-ctx)) ...) ...+)
         (define-values (stxs ids)
           (composite-pipe-helper (syntax->list #'((op arg ...) ...)) def-ctx))
         (values
          #`(composite-pipeline-member-spec
             (list #,@stxs))
          ids)]))]))

;; For first-class segments or as an escape to construct specs with the function API.
(define-syntax =pipeline-segment=
  (let ([op (λ (stx def-ctx)
              (syntax-parse stx
                [(_ segment ...)
                 (values 
                  #'(composite-pipeline-member-spec (list segment ...))
                  '())]))])
    (generics
     [core-pipeline-starter op]
     [core-pipeline-joint op])))

;; Pipe for just a single expression that isn't considered pre-wrapped in parens.
(define-syntax =basic-object-pipe/expression=
  (generics
   [core-pipeline-starter
    (λ (stx def-ctx)
      (syntax-parse stx
        [(_ e) (values #'(object-pipeline-member-spec (λ () e)) '())]))]
   [core-pipeline-joint
    (λ (stx def-ctx)
      (syntax-parse stx
        [(_ e)
         (values
          #'(object-pipeline-member-spec
             (λ (prev-ret)
               (syntax-parameterize ([current-pipeline-argument
                                      (make-rename-transformer #'prev-ret)])
                 e)))
          #;(local-expand
           #'(object-pipeline-member-spec
              (λ (prev-ret)
                (syntax-parameterize ([current-pipeline-argument
                                       (make-rename-transformer #'prev-ret)])
                  e)))
           'expression
           '()
           def-ctx)
          '())]))]))


(define-for-syntax (basic-unix-pipe-tx stx def-ctx)
  (values (basic-unix-pipe-transformer stx)
          '()))

(define-syntax =basic-unix-pipe=
  (generics
   [core-pipeline-starter basic-unix-pipe-tx]
   [core-pipeline-joint basic-unix-pipe-tx]))

(define-syntax =bind=
  (generics
   [core-pipeline-starter
    (λ (stx def-ctx)
      (syntax-parse stx
        [(~and stx (_ arg1 arg ...))
         (raise-syntax-error '=bind=
                             "Can't be used as a pipeline starter"
                             #'stx
                             #'arg1)]))]
   [core-pipeline-joint
    (λ (stx def-ctx)
      (syntax-parse stx
        [(_ name)
         (define re-name (bind! def-ctx #'name #f))
         (values
          #;#`(object-pipeline-member-spec (λ (arg) (set! #;name #,re-name arg) arg))
          (local-expand
           (qstx/rc (object-pipeline-member-spec (λ (arg) (set! #;name #,re-name arg) arg)))
           'expression
           '()
           def-ctx
           )
          (list (syntax-local-introduce re-name)))]))]))

