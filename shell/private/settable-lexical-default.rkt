#lang racket/base

#|
The point of this is to have essentially settable syntax parameters.
At some point I want to look more into syntax parameters and make a more
generic and better version.  Also I wonder why it doesn't already exist --
maybe experts will tell me later that settable syntax parameters are a
horrible idea that should never exist.  But for the niche of setting
defaults in an interactive shell, they are pretty nice...
|#

(provide define-settable-lexical-default)

(require
 racket
 racket/stxparam
 racket/splicing
 (for-syntax
  racket/base
  racket/syntax
  syntax/parse
  ))

(begin-for-syntax
  ;; define mybox, because boxes are turned into immutable boxes (rather than
  ;; 3d syntax) when turned into syntax.
  (struct mybox ([v #:mutable])
    #:transparent)
  (define (mk3dbox v)
    (datum->syntax #f (mybox v))))

(define-syntax (define-settable-lexical-default stx)
  (syntax-parse stx
    [(_ name:id default-val:id)
     (with-syntax ([getter-name (format-id #'name "get-~a" #'name)]
                   [setter-func-name (format-id #'name "set-~a/func!" #'name)]
                   [setter-macro-name (format-id #'name "set-~a!" #'name)]
                   [splicing-with-form-name (format-id #'name
                                                       "splicing-with-~a" #'name)]
                   [with-form-name (format-id #'name
                                              "with-~a" #'name)])
       #'(begin

           (define-syntax-parameter backing-syntax-parameter
             (mk3dbox #'default-val))

           (define-for-syntax (getter-name)
             (mybox-v
              (syntax->datum
               (syntax-parameter-value #'backing-syntax-parameter))))

           (define-for-syntax (setter-func-name new-val)
             (set-mybox-v!
              (syntax->datum (syntax-parameter-value #'backing-syntax-parameter))
              new-val))

           ;; TODO - if the macro to change the value is in a stop-list of local-expand,
           ;; it can cause the default to be wrong because it works via side-effect!
           ;; How can this be fixed?
           (define-syntax (setter-macro-name stx)
             (syntax-parse stx
               [(_ new-value)
                (begin
                  (setter-func-name #'new-value)
                  #'(void))]))

           (define-syntax (splicing-with-form-name stx)
             (syntax-parse stx
               [(_ new-default:id body (... ...+))
                #'(splicing-syntax-parameterize
                      ([backing-syntax-parameter
                         (mk3dbox #'new-default)])
                    body (... ...))]))

           (define-syntax (with-form-name stx)
             (syntax-parse stx
               [(_ new-default:id body ...+)
                #'(syntax-parameterize
                      ([backing-syntax-parameter
                        (mk3dbox #'new-default)])
                    body (... ...))]))))]))
