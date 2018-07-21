#lang rash

;; This isn't *really* transactional.  But with proper file system hooks to make
;; things transactional, a truly transactional expression could be this easy to run.

(provide
 call-with-transactional-file-list
 with-transactional-file-list
 )
(require
 shell/utils/bourne-expansion-utils
 racket/file
 racket/list
 (for-syntax
  racket/base
  syntax/parse
  ))

(define (move-or-copy-file src dst)
  (with-handlers ([(λ(e)#t) (λ (e) (copy-file src dst #t))])
    (rename-file-or-directory src dst #t)))

(define (call-with-transactional-file-list file-list proc)
  (if (null? file-list)
      (proc)
      (let* ([orig-file (car file-list)]
             [tmp-file (make-temporary-file "rkttmp~a" orig-file)])
        (with-handlers ([(λ (e) #t)
                         (λ (e)
                           (move-or-copy-file tmp-file orig-file)
                           (raise e))])
          (call-with-transactional-file-list (cdr file-list) proc)))))

(define-line-macro with-transactional-file-list
  (syntax-parser
    [(_ file-expr:expr ... body)
     #`(call-with-transactional-file-list
        (flatten (list #,@(map (λ (s) (dollar-expand-syntax #:glob-expand? #t s))
                               (syntax->list #'(file-expr ...)))))
        (λ () body))]))

