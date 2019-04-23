#lang racket/base

(provide

 define-pipeline-operator/no-kw
 =composite-pipe=
 =pipeline-segment=
 =basic-object-pipe/expression=
 =basic-unix-pipe=

 transform-starter-segment
 transform-joint-segment

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

  ;; For splitting macro to delimit pipeline segments
  (define-syntax-class pipeline-starter
    (pattern op:id #:when (or (core-pipeline-starter? #'(op))
                              (pipeline-starter-macro? #'(op)))))
  (define-syntax-class pipeline-joint
    (pattern op:id #:when (or (core-pipeline-joint? #'(op))
                              (pipeline-joint-macro? #'(op)))))
  (define-syntax-class not-pipeline-op
    (pattern (~and (~not x:pipeline-joint)
                   (~not x:pipeline-starter))))

  #|
  ?? Does <syntax-generic>? return #t for an identifier bound to it or to an application form using it?

  (my-generic? #'id)
  (my-generic? #'(id arg ...))

  Both, apparently!
  |#

  (define (dispatch-pipeline-starter stx)
    (cond
      ;; Do core form transformation
      [(core-pipeline-starter? stx)
       (apply-as-transformer core-pipeline-starter 'expression #f stx)]
      ;; Reduce macros to core forms
      [(pipeline-starter-macro? stx)
       (dispatch-pipeline-starter
        (apply-as-transformer pipeline-starter-macro 'expression #f stx))]
      [else (error 'dispatch-pipeline-starter "not a pipeline starter ~a\n" stx)]))
  (define (dispatch-pipeline-joint stx)
    (cond
      ;; Do core form transformation
      [(core-pipeline-joint? stx)
       (apply-as-transformer core-pipeline-joint 'expression #f stx)]
      ;; Reduce macros to core forms
      [(pipeline-joint-macro? stx)
       (dispatch-pipeline-joint
        (apply-as-transformer pipeline-joint-macro 'expression #f stx))]
      [else (error 'dispatch-pipeline-joint "not a pipeline joint ~a\n" stx)]))
  )

(define-syntax (transform-starter-segment stx)
  (syntax-parse stx [(_ arg ...) (dispatch-pipeline-starter #'(arg ...))]))
(define-syntax (transform-joint-segment stx)
  (syntax-parse stx [(_ arg ...) (dispatch-pipeline-joint #'(arg ...))]))

(define-syntax =composite-pipe=
  (generics
   [core-pipeline-starter
    (syntax-parser
      [(_ (start-op:pipeline-starter start-arg:not-pipeline-op ...)
          (join-op:pipeline-joint join-arg:not-pipeline-op ...) ...)
       #'(composite-pipeline-member-spec
          (list (transform-starter-segment start-op start-arg ...)
                (transform-joint-segment join-op join-arg ...) ...))])]
   [core-pipeline-joint
    (syntax-parser
      [(_ (op:pipeline-joint arg:not-pipeline-op ...) ...+)
       #'(composite-pipeline-member-spec
          (list (transform-joint-segment op arg ...) ...))])]))

;; For first-class segments or as an escape to construct specs with the function API.
(define-syntax =pipeline-segment=
  (let ([op (syntax-parser
              [(_ segment ...)
               #'(composite-pipeline-member-spec (list segment ...))])])
    (generics
     [core-pipeline-starter op]
     [core-pipeline-joint op])))

;; Pipe for just a single expression that isn't considered pre-wrapped in parens.
(define-syntax =basic-object-pipe/expression=
  (generics
   [core-pipeline-starter
    (syntax-parser
      [(_ e) #'(object-pipeline-member-spec (λ () e))])]
   [core-pipeline-joint
    (syntax-parser
      [(_ e)
       #'(object-pipeline-member-spec
          (λ (prev-ret)
            (syntax-parameterize ([current-pipeline-argument
                                   (make-rename-transformer #'prev-ret)])
              e)))])]))


(define-syntax =basic-unix-pipe=
  (generics
   [core-pipeline-starter basic-unix-pipe-transformer]
   [core-pipeline-joint basic-unix-pipe-transformer]))


(define-syntax define-pipeline-operator/no-kw
  (syntax-parser [(_ name as-starter as-joint outside-of-rash)
                  #'(define-syntax name
                      (generics
                       [pipeline-starter-macro as-starter]
                       [pipeline-joint-macro as-joint]
                       ;; TODO - how to do this with syntax-generics?  It was in the paper, but I don't see it in the library.
                       ;[racket-macro outside-of-rash]
                       ))]))
