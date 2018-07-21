#lang rash

;; This is testing a file written as part of a demo of better error handling, yet I'm doing shoddy error handling here.  Hmmm...


(require
 "with-transactional-file-list.rkt"
 (for-syntax
  racket/base
  syntax/parse
  ))

echo this is a test &> /tmp/wtfl-test
cp /tmp/wtfl-test /tmp/wtfl-test-a-1
cp /tmp/wtfl-test /tmp/wtfl-test-a-2
cp /tmp/wtfl-test /tmp/wtfl-test-b-1
cp /tmp/wtfl-test /tmp/wtfl-test-b-2

with-transactional-file-list /tmp/wtfl-test-a-* {
                                               echo aoeu &>! /tmp/wtfl-test-a-1
                                               }

(when (equal? #{cat /tmp/wtfl-test-a-1} #{cat /tmp/wtfl-test-a-2})
  (eprintf "failed test 1\n"))

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

(when (not (equal? #{cat /tmp/wtfl-test-b-1} #{cat /tmp/wtfl-test-b-2}))
  (eprintf "failed test 2\n"))

rm /tmp/wtfl-test*

