#lang racket/base

(provide
 define-line-macro
 cd
 )

(require
 "line-macro-detect.rkt"
 shell/pipeline-macro
 (only-in shell/pipeline path-string-symbol?)
 racket/contract
 (for-syntax
  racket/base
  syntax/parse
  "line-macro-detect.rkt"
  ))

(define-syntax (define-line-macro stx)
  (syntax-case stx ()
    [(_ name transformer)
     #'(define-syntax name (line-macro-struct transformer))]))

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
       #'(change-directory 'dir)]
      [(_) #'(change-directory (getenv "HOME"))])))

