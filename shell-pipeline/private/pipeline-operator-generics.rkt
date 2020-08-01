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
  (for/fold ([done-stx-list '()])
            ([segment segments])
    (let ([done-stx (expand-pipeline-joint segment)])
      (append done-stx-list (list done-stx)))))


(define-syntax =composite-pipe=
  (core-pipeline-op-struct
   ;; starter
   (λ (stx)
     (syntax-parse stx
       [(_ ((~var start-op (pipeline-starter))
            (~var start-arg (not-pipeline-op)) ...)
           ((~var join-op (pipeline-joint))
            (~var join-arg (not-pipeline-op)) ...) ...)
        (define stx1 (expand-pipeline-starter #'(start-op start-arg ...)))
        (define stxs2 (composite-pipe-helper (syntax->list #'((join-op join-arg ...) ...))))
        (define stxs (cons stx1 stxs2))
         #`(composite-pipeline-member-spec (list #,@stxs))]))
   ;; joint
   (λ (stx)
     (syntax-parse stx
       [(_ ((~var op (pipeline-joint))
            (~var arg (not-pipeline-op)) ...) ...+)
        (define stxs (composite-pipe-helper (syntax->list #'((op arg ...) ...))))
        #`(composite-pipeline-member-spec
            (list #,@stxs))]))))

;; For first-class segments or as an escape to construct specs with the function API.
(define-syntax =pipeline-segment=
  (let ([op (λ (stx)
              (syntax-parse stx
                [(_ segment ...)
                 #'(composite-pipeline-member-spec (list segment ...))]))])
    (core-pipeline-op-struct op op)))

;; Pipe for just a single expression that isn't considered pre-wrapped in parens.
(define-syntax =basic-object-pipe/expression=
  (core-pipeline-op-struct
   (λ (stx)
     (syntax-parse stx
       [(_ e)
        (local-expand
          #'(object-pipeline-member-spec (λ () e))
          'expression
          '()
          (current-def-ctx))]))
   (λ (stx)
     (syntax-parse stx
       [(_ e)
        (local-expand
          #'(object-pipeline-member-spec
              (λ (prev-ret)
                 (syntax-parameterize ([current-pipeline-argument
                                         (make-rename-transformer #'prev-ret)])
                                      e)))
          'expression
          '()
          (current-def-ctx))]))))


(define-for-syntax (basic-unix-pipe-tx stx)
  (basic-unix-pipe-transformer stx))

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
        (define re-name (car ((lift-binds!) #'(name) #'undefined)))
        #`(object-pipeline-member-spec (λ (arg) (debug-m (set! #,re-name arg)) arg))]))))

