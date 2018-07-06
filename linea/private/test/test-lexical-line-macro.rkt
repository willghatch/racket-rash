#lang linea "test-lang.rkt"

(module+ test
  (require rackunit)

  (define-line-macro app
    (syntax-parser [(_ arg ...) #'(#%app arg ...)]))
  (define-line-macro reversed-app
    (syntax-parser [(_ arg ...) #`(#%app #,@(reverse (syntax->list #'(arg ...))))]))

  (splicing-with-default-line-macro
   app
   (define-syntax (use-app-line-macro-implicitly stx)
     (syntax-parse stx
       [(_ arg ...)
        #'{arg ...}])))

  (splicing-with-default-line-macro
   reversed-app
   (define-syntax (use-reversed-app-line-macro-implicitly stx)
     (syntax-parse stx
       [(_ arg ...)
        #'{arg ...}])))

  (with-default-line-macro
    app
    (check-equal? (use-app-line-macro-implicitly + 5 7) 12)
    (check-equal? (use-reversed-app-line-macro-implicitly 7 5 +) 12)
    )


  )
