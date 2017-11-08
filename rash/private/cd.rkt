#lang racket/base

(provide
 cd
 )

(require
 "linea/line-macro.rkt"
 (only-in shell/pipeline path-string-symbol?)
 racket/contract
 shell/utils/bourne-expansion-utils
 (for-syntax
  racket/base
  syntax/parse
  ))

(define/contract (change-directory dir)
  (-> path-string-symbol? void?)
  (let ([ps (if (symbol? dir) (symbol->string dir) dir)])
    (if (directory-exists? ps)
        (current-directory ps)
        (error 'change-directory (format "directory doesn't exist: ~a" ps)))))

(define-line-macro cd
  (λ (stx)
    (syntax-parse stx
      [(_ (~and dir (~or ds:str di:id)))
       #`(change-directory #,(dollar-expand-syntax #'dir))]
      [(_) #'(change-directory (getenv "HOME"))])))

