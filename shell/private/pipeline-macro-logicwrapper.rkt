#lang racket/base

(provide
 run-pipeline/logic
 )

(require
 "pipeline-macro-parse.rkt"
 (prefix-in mp: "../mixed-pipeline.rkt")
 (for-syntax
  racket/base
  syntax/parse
  ))

(begin-for-syntax
  (define-syntax-class not-piperet
    (pattern (~and (~not (~literal &bg))
                   (~not (~literal &pipeline-ret)))))
  (define-syntax-class not-logic
    (pattern (~and (~not (~literal mp:and/success))
                   (~not (~literal mp:or/success)))))
  (define-syntax-class logic
    (pattern (~or (~literal mp:and/success)
                  (~literal mp:or/success))))
  )

(define-syntax (run-pipeline/logic stx)
  (syntax-parse stx
    #:literals (&bg &pipeline-ret mp:and/success mp:or/success)
    [(_ (~or (~optional (~and s-bg &bg))
             (~optional (~and s-pr &pipeline-ret)))
        ...
        arg:not-piperet ...
        (~or (~optional (~and e-bg &bg))
             (~optional (~and e-pr &pipeline-ret)))
        ...)
     (cond [(or (and (attribute s-bg)
                     (attribute e-bg))
                (and (attribute s-pr)
                     (attribute e-pr)))
            (raise-syntax-error
             'run-pipeline/logic
             "duplicated occurences of pipeline options at beginning and end"
             stx)]
           [(or (attribute s-bg) (attribute e-bg))
            (raise-syntax-error
             'run-pipeline/logic
             "bg not yet supported")]
           [(or (attribute s-pr) (attribute e-pr))
            #'(run-pipeline/logic/inner arg ...)]
           [else
            #'(let* ([pline (run-pipeline/logic/inner arg ...)]
                     [ret (mp:pipeline-return pline)])
                (or (and (mp:pipeline-success? pline)
                         ret)
                    (raise ret)))])]))

(define-syntax (run-pipeline/logic/inner stx)
  (syntax-parse stx
    [(_ a:not-logic ... op:logic b:not-logic ... rest ...)
     #'(run-pipeline/logic/helper
        (op (run-pipeline &pipeline-ret a ...)
            (run-pipeline &pipeline-ret b ...))
        rest ...)]
    [(_ a:not-logic ...)
     #'(run-pipeline &pipeline-ret a ...)]))

(define-syntax (run-pipeline/logic/helper stx)
  (syntax-parse stx
    [(rec prev op:logic b:not-logic ... rest ...)
     #'(rec
        (op prev
            (run-pipeline &pipeline-ret b ...))
        rest ...)]
    [(_ prev)
     #'prev]))
