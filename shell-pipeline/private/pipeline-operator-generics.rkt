#lang racket/base

(provide

 define-pipeline-operator/no-kw
 =composite-pipe=
 =pipeline-segment=
 =basic-object-pipe/expression=
 =basic-unix-pipe=
 =bind=

 current-pipeline-argument

 (for-syntax

  pipeline-starter
  pipeline-joint
  not-pipeline-op

  expand-pipeline-starter
  expand-pipeline-joint

  core-pipeline-starter
  core-pipeline-joint

  gen:macro-pipeline-op

  ))

(require
  "basic-unix-pipe-helper-funcs.rkt"
  "mostly-structs.rkt"
  "block.rkt"
  racket/stxparam
  racket/undefined
  (for-syntax
   racket/base
   syntax/parse
   racket/generic
   ee-lib
   ;; for pretty-print debug
   racket/pretty
   ))

(define-syntax-parameter current-pipeline-argument
  (λ (stx) (raise-syntax-error 'current-pipeline-argument
                               "Can't use implicit pipeline argument here."
                               stx)))


(begin-for-syntax
  (define-generics core-pipeline-op
    (core-pipeline-starter core-pipeline-op stx)
    (core-pipeline-joint core-pipeline-op stx))
  (struct core-pipeline-op-struct (starter-transformer joint-transformer)
    #:property prop:procedure
    (λ (op stx)
      (raise-syntax-error #f stx
                          "Must be used as a pipeline operator"))
    #:methods gen:core-pipeline-op
    [(define (core-pipeline-starter op stx)
       ((core-pipeline-op-struct-starter-transformer op) stx))
     (define (core-pipeline-joint op stx)
       ((core-pipeline-op-struct-joint-transformer op) stx))])

  (define-generics macro-pipeline-op
    (macro-pipeline-starter macro-pipeline-op stx)
    (macro-pipeline-joint macro-pipeline-op stx))
  (struct macro-pipeline-op-struct (starter-transformer joint-transformer as-macro)
    #:property prop:procedure (struct-field-index as-macro)
    #:methods gen:macro-pipeline-op
    [(define (macro-pipeline-starter op stx)
       ((macro-pipeline-op-struct-starter-transformer op) stx))
     (define (macro-pipeline-joint op stx)
       ((macro-pipeline-op-struct-joint-transformer op) stx))])


  ;; For splitting macro to delimit pipeline segments
  (define-syntax-class -core-pipeline-op
    (pattern op:id #:when (core-pipeline-op? (lookup #'op))))
  (define-syntax-class -macro-pipeline-op
    (pattern op:id #:when (macro-pipeline-op? (lookup #'op))))

  (define-syntax-class pipeline-op
    (pattern (~or _:-core-pipeline-op _:-macro-pipeline-op)))
  ;; TODO - these are used everywhere in the interface, but could be simplified to just pipeline-op.
  (define-syntax-class pipeline-starter
    (pattern (~or _:-core-pipeline-op _:-macro-pipeline-op)))
  (define-syntax-class pipeline-joint
    (pattern (~or _:-core-pipeline-op _:-macro-pipeline-op)))
  (define-syntax-class (not-pipeline-op)
    (pattern (~not x:pipeline-op)))



  #|
  For binding, I need the core desugaring thing to return the syntax to make a pipeline-member-spec, but also the names of any bindings (in both original and transformed form).  Then the next pipeline segment will need to be in a new context that can see that binding and has it bound to... maybe a transformer that will get a value out of a box?  And the spec form for that binding will need to run code that sets that box.  So I need to generate a getter and setter, and in the binding-spec-generator expression I need to let-syntax it to the setter, and further in the pipeline I need to let-syntax it to the getter.  Except I also want to be able to set! it later, so I need the getter to be a set!-transformer.
  Then the outer driver thing will have the original and transformed binding names, and can arrange all the let-syntax stuff as well as an outer definition of the given names if it is in a definition context.

  What arguments does it need to accept?  It needs the syntax object itself, obviously.  It needs a definition context, I guess?  Does it need a new one for each new expansion?  They should probably be parent/child definition contexts.

  (-> stx definition-context (values syntax (listof id)))
  |#

  #|
  TODO - These #:expression keywords on the 4 define/hygienic uses should be #:definition, but there seems to be some bug.  It is currently working with #:expression and not with #:definition.
  |#

  (define/hygienic (expand-pipeline-starter stx) #:definition
    (syntax-parse stx
      [(op:-core-pipeline-op arg ...)
       (core-pipeline-starter (lookup #'op) stx)]
      [(op:-macro-pipeline-op arg ...)
       (expand-pipeline-starter
        (macro-pipeline-starter (lookup #'op)
                                stx))]
      [else (error 'expand-pipeline-starter "not a pipeline starter ~a\n" stx)]))
  (define/hygienic (expand-pipeline-joint stx) #:definition
    (syntax-parse stx
      [(op:-core-pipeline-op arg ...)
       (core-pipeline-joint (lookup #'op) stx)]
      [(op:-macro-pipeline-op arg ...)
       (expand-pipeline-joint
        (macro-pipeline-joint (lookup #'op)
                              stx))]
      [else (error 'expand-pipeline-joint "not a pipeline joint ~a\n" stx)]))
  )

;; basic definition form, wrapped by the better one in "pipeline-operators.rkt"
(define-syntax define-pipeline-operator/no-kw
  (syntax-parser
    [(_ name as-starter as-joint outside-of-rash)
     #'(define-syntax name
         (macro-pipeline-op-struct as-starter as-joint outside-of-rash))]))


(define-for-syntax (composite-pipe-helper segments)
  (for/fold ([done-stx-list '()]
             [lifted-ids '()])
            ([segment segments])
    (let-values ([(done-stx ids) (expand-pipeline-joint segment)])
      (values (append done-stx-list (list done-stx))
              (append lifted-ids ids)))))


(define-syntax =composite-pipe=
  (core-pipeline-op-struct
   ;; starter
   (λ (stx)
     (syntax-parse stx
       [(_ ((~var start-op (pipeline-starter))
            (~var start-arg (not-pipeline-op)) ...)
           ((~var join-op (pipeline-joint))
            (~var join-arg (not-pipeline-op)) ...) ...)
        (define-values (stx1 ids1)
          (expand-pipeline-starter #'(start-op start-arg ...)))
        (define-values (stxs2 ids2)
          (composite-pipe-helper (syntax->list #'((join-op join-arg ...) ...))))
        (define stxs (cons stx1 stxs2))
        (define ids (append ids1 ids2))
        (values
         #`(composite-pipeline-member-spec (list #,@stxs))
         ids)]))
   ;; joint
   (λ (stx)
     (syntax-parse stx
       [(_ ((~var op (pipeline-joint))
            (~var arg (not-pipeline-op)) ...) ...+)
        (define-values (stxs ids)
          (composite-pipe-helper (syntax->list #'((op arg ...) ...))))
        (values
         #`(composite-pipeline-member-spec
            (list #,@stxs))
         ids)]))))

;; For first-class segments or as an escape to construct specs with the function API.
(define-syntax =pipeline-segment=
  (let ([op (λ (stx)
              (syntax-parse stx
                [(_ segment ...)
                 (values 
                  #'(composite-pipeline-member-spec (list segment ...))
                  '())]))])
    (core-pipeline-op-struct op op)))

;; Pipe for just a single expression that isn't considered pre-wrapped in parens.
(define-syntax =basic-object-pipe/expression=
  (core-pipeline-op-struct
   (λ (stx)
     (syntax-parse stx
       [(_ e) (values (local-expand
                       #'(object-pipeline-member-spec (λ () e))
                       'expression
                       '()
                       (current-def-ctx))
                      '())]))
   (λ (stx)
     (syntax-parse stx
       [(_ e)
        (values
         #;#'(object-pipeline-member-spec
              (λ (prev-ret)
                (syntax-parameterize ([current-pipeline-argument
                                       (make-rename-transformer #'prev-ret)])
                  e)))
         (local-expand
          #'(object-pipeline-member-spec
             (λ (prev-ret)
               (syntax-parameterize ([current-pipeline-argument
                                      (make-rename-transformer #'prev-ret)])
                 e)))
          'expression
          '()
          (current-def-ctx))
         '())]))))


(define-for-syntax (basic-unix-pipe-tx stx)
  (values (basic-unix-pipe-transformer stx) '()))

(define-syntax =basic-unix-pipe=
  (core-pipeline-op-struct basic-unix-pipe-tx basic-unix-pipe-tx))

(define-syntax debug-m
  (syntax-parser
    [(_ (set! name e))
     ;(pretty-print (syntax-debug-info #'name))
     #`(set! name e)]))
(define-syntax =bind=
  ;; starter
  (core-pipeline-op-struct
   (λ (stx)
     (syntax-parse stx
       [(~and stx (_ arg1 arg ...))
        (raise-syntax-error '=bind=
                            "Can't be used as a pipeline starter"
                            #'stx
                            #'arg1)]))
   ;; joint
   (λ (stx)
     (syntax-parse stx
       [(_ name)
        ;(define re-name (bind! #'name #f))
        (define re-name (car ((lift-binds!) #'(name) #'undefined)))
        (values
         #`(object-pipeline-member-spec (λ (arg) (debug-m (set! #,re-name arg)) arg))
         #;(list (syntax-local-identifier-as-binding
                (syntax-local-introduce re-name)))
         '()
         )]))))

