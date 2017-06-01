#lang racket/base

(provide
 (for-syntax
  ;prop:rash-pipeline-starter rash-pipeline-starter? rash-pipeline-starter-ref
  ;prop:rash-pipeline-joiner rash-pipeline-joiner? rash-pipeline-joiner-ref
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

 current-rash-pipeline-argument

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

 &bg &pipeline-ret &env &env-replace &in &< &out &> &>! &>> &err

 ;; semi-private -- these are not re-provided
 rash-pipeline-splitter
 rash-pipeline-opt-hash
 default-output-port-transformer
 )

(require
 racket/stxparam
 racket/string
 shell/mixed-pipeline
 racket/port
 (for-syntax
  racket/base
  racket/match
  syntax/parse
  racket/stxparam-exptime
  (for-syntax
   racket/base
   syntax/parse
   )))

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
    [(_ arg ...+) #'(u-pipeline-member-spec (list arg ...) 'default)])
  #:joint
  (syntax-parser
    [(_ arg ...+) #'(u-pipeline-member-spec (list arg ...) 'default)]))

(define-rash-pipe =crappy-basic-unix-pipe=
  #:start
  (syntax-parser
    [(_ arg ...+) #'(u-pipeline-member-spec '(arg ...) 'default)])
  #:joint
  (syntax-parser
    [(_ arg ...+) #'(u-pipeline-member-spec '(arg ...) 'default)]))


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

(define-rash-pipe =for/list=
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

(define-syntax (def-pipeline-opt stx)
  (syntax-parse stx
    [(_ name)
     #'(define-syntax-parameter name
         (syntax-id-rules
          ()
          [e (raise-syntax-error
              'name
              "can't be used except at the beginning of a rash pipeline specification"
              #'e)]))]))
(def-pipeline-opt &bg)
(def-pipeline-opt &pipeline-ret) ;; TODO - name?
(def-pipeline-opt &env)
(def-pipeline-opt &env-replace)
(def-pipeline-opt &in)
(def-pipeline-opt &<)
(def-pipeline-opt &out)
(def-pipeline-opt &>)
(def-pipeline-opt &>!)
(def-pipeline-opt &>>)
(def-pipeline-opt &err)


(define rash-pipeline-opt-hash (make-parameter (hash)))
(define (ropt key)
  (hash-ref (rash-pipeline-opt-hash) key))

(begin-for-syntax
  (define-syntax-class not-opt
    #:literals (&bg &pipeline-ret &env &env-replace &in &out &err &< &> &>! &>>)
    (pattern (~not (~or &bg &pipeline-ret &env &env-replace &in &out &err &< &> &>! &>>)))))

(define-syntax (rash-pipeline-splitter stx)
  ;; Parse out the beginning/ending whole-pipeline options, then
  ;; pass the pipeline specs to other macros to deal with.
  (syntax-parse stx
    [(_ (outer-in outer-out outer-err) rash-arg ...)
     (syntax-parse #'(rash-arg ...)
       #:literals (&bg &pipeline-ret &env &env-replace &in &out &err &< &> &>! &>>)
       [((~or (~optional (~and s-bg &bg) #:name "&bg option")
              (~optional (~and s-pr &pipeline-ret) #:name "&pipeline-ret option")
              (~optional (~seq &env s-env-list:expr) #:name "&env option")
              (~optional (~seq &env-replace s-env-r-list:expr) #:name "&env-replace option")
              (~optional (~or (~seq &in s-in:expr)
                              (~seq &< s-<:expr))
                         #:name "&in and &< options")
              (~optional (~or (~seq &out s-out:expr)
                              (~seq &> s->:expr)
                              (~seq &>! s->!:expr)
                              (~seq &>> s->>:expr))
                         #:name "&out, &>, &>!, and &>> options")
              (~optional (~seq &err s-err:expr) #:name "&err option")
              )
         ...
         args1-head:not-opt args1 ...)
        ;; Now let's parse those in reverse at the end, so options are allowed at the beginning OR the end
        (syntax-parse (datum->syntax stx (reverse (syntax->list #'(args1-head args1 ...))))
          #:literals (&bg &pipeline-ret &env &env-replace &in &out &err &< &> &>! &>>)
          [((~or (~optional (~and e-bg &bg) #:name "&bg option")
                 (~optional (~and e-pr &pipeline-ret) #:name "&pipeline-ret option")
                 (~optional (~seq e-env-list:expr &env) #:name "&env option")
                 (~optional (~seq e-env-r-list:expr &env-replace) #:name "&env-replace option")
                 (~optional (~or (~seq e-in:expr &in)
                                 (~seq e-<:expr &<))
                            #:name "&in and &< options")
                 (~optional (~or (~seq e-out:expr &out)
                                 (~seq e->:expr &>)
                                 (~seq e->!:expr &>!)
                                 (~seq e->>:expr &>>))
                            #:name "&out, &>, &>!, and &>> options")
                 (~optional (~seq e-err:expr &err) #:name "&err option")
                 )
            ...
            argrev-head:not-opt argrev ...)
           (syntax-parse (datum->syntax stx (reverse (syntax->list #'(argrev-head argrev ...))))
             [(arg ...)
              ;; Let's go more meta to clean up some error checking code...
              (define-syntax (noboth stx1)
                (syntax-parse stx1
                  [(_ (a ...) (b ...))
                   #'(when (and (or (attribute a) ...) (or (attribute b) ...))
                       (raise-syntax-error
                        'rash-pipeline-splitter
                        "duplicated occurences of pipeline options at beginning and end"
                        stx))]
                  [(rec a:id b:id)
                   #'(rec (a) (b))]))
              (noboth s-bg e-bg)
              (noboth s-pr e-pr)
              (noboth s-env-list e-env-list)
              (noboth s-env-r-list e-env-r-list)
              (noboth (s-in s-<) (e-in e-<))
              (noboth (s-out s-> s->! s->>) (e-out e-> e->! e->>))
              (noboth s-err e-err)
              #`(parameterize ([rash-pipeline-opt-hash
                                (hash 'bg #,(if (or (attribute s-bg) (attribute e-bg))
                                                #'#t #'#f)
                                      'pipeline-ret #,(if (or (attribute s-pr) (attribute e-pr))
                                                          #'#t #'#f)
                                      'env #,(cond [(attribute s-env-list)]
                                                   [(attribute e-env-list)]
                                                   [else #''()])
                                      'env-replace #,(cond [(attribute s-env-r-list)]
                                                           [(attribute e-env-r-list)]
                                                           [else #'#f])
                                      'in #,(cond [(attribute s-in)]
                                                  [(attribute e-in)]
                                                  [(attribute s-<) #`(quote #,(attribute s-<))]
                                                  [(attribute e-<) #`(quote #,(attribute e-<))]
                                                  ;; TODO - respect outer macro default
                                                  [else #'outer-in])
                                      'out #,(cond [(attribute s-out)]
                                                   [(attribute e-out)]
                                                   [(attribute s->) #`(list (quote #,(attribute s->))
                                                                            'error)]
                                                   [(attribute e->) #`(list (quote #,(attribute e->))
                                                                            'error)]
                                                   [(attribute s->!) #`(list (quote #,(attribute s->!))
                                                                             'truncate)]
                                                   [(attribute e->!) #`(list (quote #,(attribute e->!))
                                                                             'truncate)]
                                                   [(attribute s->>) #`(list (quote #,(attribute s->>))
                                                                             'append)]
                                                   [(attribute e->>) #`(list (quote #,(attribute e->>))
                                                                             'append)]
                                                   ;; TODO - respect outer macro default
                                                   [else  #'outer-out])
                                      'err #,(cond [(attribute s-err)]
                                                   [(attribute s-err)]
                                                   ;; TODO - respect outer macro default
                                                   [else #'outer-err])
                                      )])
                  (rash-pipeline-splitter/start rash-pipeline-splitter/done/do arg ...))])])])]))

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
        #:bg (ropt 'bg)
        #:in (ropt 'in) #:out (ropt 'out) #:err (ropt 'err)
        (rash-transform-starter-segment starter startarg ...)
        (rash-transform-joiner-segment joiner joinarg ...) ...)]))

(define-syntax (rash-do-pipeline stx)
  ;; TODO -- parameterize option hash
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

(define default-output-port-transformer (λ (p) (string-trim (port->string p))))

;; TODO - implement for real
(define (rash-do-transformed-pipeline #:bg [bg #f]
                                      #:in [in (open-input-string "")]
                                      #:out [out default-output-port-transformer]
                                      #:err [err 'string-port]
                                      . args)
  ;; TODO - environment extension/replacement
  (run-pipeline args #:bg bg #:in in #:out out #:default-err err))
