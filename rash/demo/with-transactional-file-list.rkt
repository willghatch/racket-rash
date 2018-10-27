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

(module+ non-sandboxed-test
  {
   ;; This is testing a file written as part of a demo of better error handling, yet I'm doing shoddy error handling here.  Hmmm...
   (require rackunit (for-syntax racket/base syntax/parse))

   echo this is a test &> /tmp/wtfl-test
   cp /tmp/wtfl-test /tmp/wtfl-test-a-1
   cp /tmp/wtfl-test /tmp/wtfl-test-a-2
   cp /tmp/wtfl-test /tmp/wtfl-test-b-1
   cp /tmp/wtfl-test /tmp/wtfl-test-b-2

   with-transactional-file-list /tmp/wtfl-test-a-* {
                                                    echo aoeu &>! /tmp/wtfl-test-a-1
                                                    }

   (check-not-equal? #{cat /tmp/wtfl-test-a-1} #{cat /tmp/wtfl-test-a-2})

   (define-line-macro try
     (syntax-parser [(_ body (~datum catch) catch-body)
                     #'(with-handlers ([(λ (e) #t) (λ (e) catch-body)])
                         body)]))


   try {
        with-transactional-file-list /tmp/wtfl-test-b-* {
                                                         echo aoeu &>! /tmp/wtfl-test-b-1
                                                         (error 'foo)
                                                         }
        } catch {
                 (void)
                 }

   (check-equal? #{cat /tmp/wtfl-test-b-1} #{cat /tmp/wtfl-test-b-2})

   rm /tmp/wtfl-test*
   })
