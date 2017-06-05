#lang racket/base

(provide
 define-pipeline-operator

 current-pipeline-argument

 (for-syntax expand-pipeline-arguments)

 =composite-pipe=

 =basic-object-pipe=
 =basic-object-pipe/left=
 =basic-object-pipe/expression=
 =object-pipe=
 =object-pipe/left=
 =object-pipe/expression=

 =for/list=

 =non-quoting-basic-unix-pipe=
 =crappy-basic-unix-pipe=
 )

(require
 racket/stxparam
 racket/port
 shell/mixed-pipeline
 "pipeline-operator-transform.rkt"
 (for-syntax
  racket/base
  syntax/parse
  "pipeline-operator-detect.rkt"
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


;;;;;;;;;;;;;;;; Pipeline argument detection, replacement functions

(define-syntax-parameter current-pipeline-argument #f)
;; placeholder value so local-expand doesn't barf
(define rash-pipeline-argument-standin #f)

(define-for-syntax (stx-replace stx from-id to-id)
  ;; Replace from-id with to-id recursively.
  ;; If the result is eq? to stx, then no change was made.
  ;; If the result is NOT eq? to stx, then at least one change was made.

  #|
  TODO - replace this whole function with an abstracted version from a library at some point.  If I have to make it myself, I can look in eg. syntax-strip-context and syntax-replace-context, as well as syntax-map from the racket implementation of the expander.
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
     (with-syntax ([prev-arg #'rash-pipeline-argument-standin])
       (let ([e-args
              (map (λ (s) (local-expand
                           #`(syntax-parameterize
                                 ([current-pipeline-argument
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

;;;; object pipes

(define-pipeline-operator =basic-object-pipe=
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
(define-pipeline-operator =basic-object-pipe/left=
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

(define-pipeline-operator =basic-object-pipe/expression=
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

(define-pipeline-operator =object-pipe=
  #:start (syntax-parser [(_ arg ...+) #'(=basic-object-pipe= arg ...)])
  #:joint
  (syntax-parser [(_ arg ...+) (with-port-sugar #'(=basic-object-pipe= arg ...))]))
(define-pipeline-operator =object-pipe/left=
  #:start (syntax-parser [(_ arg ...+) #'(=basic-object-pipe/left= arg ...)])
  #:joint
  (syntax-parser [(_ arg ...+) (with-port-sugar #'(=basic-object-pipe/left= arg ...))]))
(define-pipeline-operator =object-pipe/expression=
  #:start (syntax-parser [(_ e) #'(=basic-object-pipe/expression= e)])
  #:joint
  (syntax-parser [(_ e) (with-port-sugar #'(=basic-object-pipe/expression= e))]))

(define-pipeline-operator =for/list=
  #:joint
  (syntax-parser
    [(_ arg ...+)
     (expand-pipeline-arguments
        #'(arg ...)
        #'for-iter
        (syntax-parser
          [(#t narg ...)
           #'(obj-pipeline-member-spec (λ (prev-ret)
                                         (for/list ([for-iter prev-ret])
                                           (narg ...))))]
          [(#f narg ...)
           #'(obj-pipeline-member-spec (λ (prev-ret)
                                         (for/list ([for-iter prev-ret])
                                           (narg ... for-iter))))]))]))


;;;; unix-y pipes

(define-pipeline-operator =non-quoting-basic-unix-pipe=
  #:start
  (syntax-parser
    [(_ arg ...+) #'(u-pipeline-member-spec (list arg ...) 'default)])
  #:joint
  (syntax-parser
    [(_ arg ...+) #'(u-pipeline-member-spec (list arg ...) 'default)]))

(define-pipeline-operator =crappy-basic-unix-pipe=
  #:start
  (syntax-parser
    [(_ arg ...+) #'(u-pipeline-member-spec '(arg ...) 'default)])
  #:joint
  (syntax-parser
    [(_ arg ...+) #'(u-pipeline-member-spec '(arg ...) 'default)]))

