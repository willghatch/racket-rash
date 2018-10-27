#lang rash
(provide
 call-with-tmp-dir
 with-tmp-dir
 in-tmp-dir
 )
(require
 racket/file
 rash/demo/setup ;; for in-dir
 (for-syntax
  racket/base
  syntax/parse
  ))

(define (call-with-tmp-dir proc)
  (define td (make-temporary-file "call-with-tmp-dir-~a"
                                  'directory))
  (define result
    (with-handlers ([(λ (e) #t) (λ (e)
                                  (delete-directory/files td #:must-exist? #f)
                                  (raise e))])
      (proc td)))
  (delete-directory/files td #:must-exist? #f)
  result)

(define-line-macro with-tmp-dir
  (syntax-parser
    [(_ dir-name-var:id body:expr)
     #'(call-with-tmp-dir (λ (dir-name-var) body))]))

(define-line-macro in-tmp-dir
  (syntax-parser
    [(_ body:expr)
     #'(with-tmp-dir d (in-dir (values d) body))]))

(module+ non-sandboxed-test
  {

   (require rackunit)

   (define orig-dir (current-directory))
   (define dir1 #f)
   (define dir2 #f)
   with-tmp-dir d {
                   (check-true (directory-exists? d))
                   echo $d
                   echo hello foo bar &> $d/file
                   echo $d/file
                   cat $d/file
                   (set! dir1 d)
                   }

   (check-false (directory-exists? dir1))

   in-tmp-dir {
               (set! dir2 (current-directory))
               echo in (current-directory)
               }

   (check-false (directory-exists? dir2))
   (check-false (equal? dir1 dir2))

   })
