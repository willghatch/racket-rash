#lang racket/base

#|
This is a demo `make` replacement language.

An example usage would look like this:

```
#lang rash/demo/make
(define cc 'gcc)
hello : hello.c {
  $cc -o (current-target) (current-dependencies)
}
```

Targets and dependencies can be computed (use $dollar-vars or parenthesized
s-expressions), current-target and current-dependencies parameters are
available in the recipe bodies, and command-line usage with --help that
lists available targets is generated automatically.

The `make-module-begin` does half of the heavy lifting, along with the
`make-target` line-macro, which `make-module-begin` sets as the default.
The `make-target` line-macro sets the normal `run-pipeline` as the default
inside the recipe body.
|#

(provide
 (except-out (all-from-out racket/base)
             #%module-begin)
 (all-from-out rash)
 (rename-out [make-module-begin #%module-begin])

 make-target
 current-target
 current-dependencies
 )


(require
 rash
 make
 racket/cmdline
 racket/match
 racket/list
 racket/stxparam
 racket/splicing
 shell/utils/bourne-expansion-utils
 (for-syntax
  racket/base
  syntax/parse
  ))

(make-rash-reader-submodule rash/make)

(define-syntax make-module-begin
  (syntax-parser
    [(_ form:expr ...+)
     #'(#%module-begin
        (define the-target-list-box (box '()))
        (splicing-syntax-parameterize
            ([target-list-box (syntax-parser [x:id #'the-target-list-box])])
          (splicing-with-rash-config
           #:out (current-output-port)
           #:in (current-input-port)
           #:err (current-error-port)
           #:starter =unix-pipe=
           #:line-macro make-target
           form
           ...
           (do-make))))]))

(define current-target (make-parameter #f))
(define current-dependencies (make-parameter '()))

(define (target/dep-stringify td)
  (match td
    [(list inner-td ...)
     (map target/dep-stringify (flatten inner-td))]
    [(? string?) td]
    [(? symbol?) (symbol->string td)]
    [else (error 'make-target
                 "received a bad target or dependency specification -- expected string, symbol, or list of strings and symbols: ~s"
                 td)]))

(define (make-make-list target deps body-func)
  (list target deps (λ () (parameterize
                              ([current-target target]
                               [current-dependencies deps])
                            (body-func)))))

(define-syntax-parameter target-list-box (syntax-parser))

(define-line-macro make-target
  (syntax-parser
    #:datum-literals (:)
    [(_ target:expr ...+
        :
        dependency:expr ...
        body:expr)
     (define (target/dep-quote-maybe stx)
       (syntax-parse stx
         [(e1:expr e ...) stx]
         [else (dollar-expand-syntax stx #:glob-expand? #t)]))
     (define target-exprs
       (map target/dep-quote-maybe (syntax->list #'(target ...))))
     (define deps-exprs
       (map target/dep-quote-maybe (syntax->list #'(dependency ...))))
     #`(set-box!
        target-list-box
        (append
         (let ([target-result (target/dep-stringify (list #,@target-exprs))]
               [deps-result (target/dep-stringify (list #,@deps-exprs))]
               [body-result (λ () (with-rash-config
                                    #:line-macro run-pipeline
                                    body))])
           (if (list? target-result)
               (map (λ (t) (make-make-list t deps-result body-result))
                    target-result)
               (list (make-make-list target-result deps-result body-result))))
         (unbox target-list-box)))]))

(define-line-macro do-make
  (syntax-parser
    [(_)
     #`(let* ([make-lists (reverse (unbox target-list-box))]
              [targets (map car make-lists)]
              [targets-to-make
               (parse-command-line
                "make" (current-command-line-arguments)
                `((ps
                   "Available-targets:"
                   ,@targets))
                (λ (flag-accum . chosen-targets) chosen-targets)
                '("chosen-target ..."))])
         (make/proc make-lists targets-to-make))]))
