#lang rash

(provide
 infix-math
 pipeline-or-math
 =infix-math=
 =unix-or-math=
 )

(require
 (rename-in "basic-infix-math.rkt" [infix-math basic-infix-math])
 racket/stxparam
 (for-syntax
  racket/base
  syntax/parse
  ))

(define-line-macro infix-math
  (syntax-parser [(_ arg ...) #'(basic-infix-math arg ...)]))

(define-line-macro pipeline-or-math
  (syntax-parser [(_ arg1:number arg ...)
                  #'(basic-infix-math arg1 arg ...)]
                 [(_ arg ...)
                  #'(run-pipeline arg ...)]))

(define-pipeline-operator =infix-math=
  #:start (syntax-parser [(_ arg ...) #'(object-pipeline-member-spec
                                         (λ () (basic-infix-math arg ...)))])
  #:joint (syntax-parser [(_ arg ...) #'(object-pipeline-member-spec
                                         (λ (x) (syntax-parameterize
                                                    ([current-pipeline-argument
                                                      (syntax-parser [_ #'x])])
                                                  (basic-infix-math arg ...))))]))
(pipeop =unix-or-math=
        [(_ arg1:number arg ...) #'(=infix-math= arg1 arg ...)]
        [(_ arg ...) #'(=unix-pipe= arg ...)])


