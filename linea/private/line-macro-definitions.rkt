#lang racket/base

;; struct properties and syntax classes for defining and detecting line macros

(provide
 #%linea-line
 #%linea-line/context
 #%linea-s-exp
 #%linea-expressions-begin
 #%linea-default-line-macro

 define-line-macro
 with-default-line-macro
 splicing-with-default-line-macro
 )

(require
 racket/stxparam
 racket/splicing
 (for-syntax
  "line-macro-detect.rkt"
  racket/base
  syntax/parse
  ))


(define-syntax (define-line-macro stx)
  (syntax-case stx ()
    [(_ name transformer)
     #'(define-syntax name (line-macro-struct transformer))]))

(define-for-syntax (dlm ctxt loc)
  (datum->syntax ctxt '#%linea-default-line-macro loc))

(define-syntax (#%linea-line/context stx)
  (syntax-parse stx
    [(_ context arg1:line-macro arg ...)
     (linea-line-macro-transform
      (datum->syntax #'context (syntax->list #'(arg1 arg ...))))]
    [(rec context arg ...)
     (let ([default-macro (dlm #'context #'context)])
       (syntax-parse default-macro
         [default-line-macro:line-macro
           #'(rec context default-line-macro arg ...)]
         [_ (raise-syntax-error
             '#%linea-line
             "A line of Linea (Rash) code was used without an explicit line macro and #%linea-default-line-macro is not bound or not bound to a line macro"
             #'rec)]))]))

(define-syntax (#%linea-line stx)
  ;; detect line macros and apply them, or transform into pipeline
  (syntax-parse stx
    [(_ arg ...)
     (with-syntax ([context (datum->syntax stx #f)])
       #'(#%linea-line/context context arg ...))]))

(define-syntax #%linea-expressions-begin (make-rename-transformer #'begin))

;;; #%linea-s-exp is just a pass-through -- it's what wraps normal Racket forms
;;; when they are completely escaped from the line syntax.
(define-syntax (#%linea-s-exp stx)
  (syntax-parse stx [(_ e) #'e]))

(define-line-macro #%linea-default-line-macro
  (λ (stx)
    (syntax-parse stx
      [(_ arg ...)
       (define msg
         "The base default for #%linea-default-line-macro is to raise this syntax error.  Perhaps your #%module-begin didn't set a different one.")
       (define args (syntax->list #'(arg ...)))
       (if (null? args)
           (raise-syntax-error
            '#%linea-default-line-macro
            msg
            stx)
           (raise-syntax-error
            '#%linea-default-line-macro
            msg
            stx
            (car args)
            (cdr args)))])))

(define-for-syntax (with-default-line-macro* stx #:context [context #f])
  (syntax-parse stx
    [(orig-macro let-form new-default:line-macro e ...)
     (define default-line-macro-stx
       (or (and context (dlm context #'orig-macro))
           (let ([dlms (map (λ (x) (dlm x #'orig-macro))
                            (cond [(null? (attribute e)) (list #'here)]
                                  [else (attribute e)]))])
             (unless (for/and ([x (cdr dlms)])
                       (bound-identifier=? (car dlms) x))
               (raise-syntax-error
                'with-default-line-macro
                "Multiple body forms were given with different scoping information, so there is not a clear choice of info to bind the default line macro to."
                #'new-default))
             (car dlms))))
     (with-syntax ([default-line-macro default-line-macro-stx])
       #'(let-form ([default-line-macro (make-rename-transformer
                                         (quote-syntax new-default))])
                   e ...))]))

(define-line-macro with-default-line-macro
  (syntax-parser
    [(w-d-l-m (~optional (~seq #:context context)) new-default:line-macro e ...+)
     (with-default-line-macro*
       #:context (attribute context)
       #'(w-d-l-m let-syntax new-default e ...))]))
(define-line-macro splicing-with-default-line-macro
  (syntax-parser
    [(w-d-l-m (~optional (~seq #:context context)) new-default:line-macro e ...+)
     (with-default-line-macro*
       #:context (attribute context)
       #'(w-d-l-m splicing-let-syntax new-default e ...))]))

(module+ test
  (require rackunit)
  (define-line-macro app (syntax-parser [(_ op e ...) #'(#%app op e ...)]))
  (define-line-macro rpn-app (syntax-parser [(_ e ... op) #'(#%app op e ...)]))

  (check-equal? (with-default-line-macro
                  app
                  (#%linea-expressions-begin
                   (#%linea-line + 4 5 6)))
                15)
  (check-equal? (with-default-line-macro
                  app
                  (#%linea-expressions-begin
                   (#%linea-line rpn-app 4 5 6 +)))
                15)
  (check-equal? (splicing-with-default-line-macro
                  app
                  (#%linea-expressions-begin
                   (#%linea-line + 4 5 6)))
                15)
  )
