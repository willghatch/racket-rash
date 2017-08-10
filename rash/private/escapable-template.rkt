#lang racket/base

(provide escapable-template quasi-escapable-template)

(require
 (for-syntax
  racket/base
  syntax/parse
  "syntax-walk.rkt"
  "template-escape-detect.rkt"
  ))

(define-for-syntax (stoppable-expand-escapes-walker stop-list)
  (λ (stx do-rec)
    (syntax-parse stx
      [(escaper:template-escape arg ...)
       (template-escape-transform #'(escaper arg ...))]
      [(maybe-stop arg ...)
       (if (ormap (λ (id) (free-identifier=? id #'maybe-stop)) stop-list)
           stx
           (do-rec))]
      [_ (do-rec)])))

;; TODO - these should probably recur on the output of the transformations as well.
(define-syntax (escapable-template stx)
  (syntax-parse stx
    [(_ template-form)
     (with-syntax ([walked (syntax-walk (stoppable-expand-escapes-walker '())
                                        #'template-form)])
       #'(syntax walked))]))

(define-syntax (quasi-escapable-template stx)
  (syntax-parse stx
    [(_ template-form)
     (with-syntax ([walked (syntax-walk (stoppable-expand-escapes-walker
                                         (list #'unsyntax #'unsyntax-splicing))
                                        #'template-form)])
       #'(quasisyntax walked))]))
