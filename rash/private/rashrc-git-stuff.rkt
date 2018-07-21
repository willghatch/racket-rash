#lang rash

(provide

 timeout->default

 get-git-info

 get-git-root
 get-current-git-branch
 git-behind/ahead-numbers
 git-dirty?
 git-submodule-dirty?
 git-has-untracked?
 )

(require
 (for-syntax
  racket/base
  syntax/parse
  ))

(define git-timeout-flag (gensym))
(define (timeout->default v default)
  (if (eq? git-timeout-flag v)
      default
      v))

(define-syntax (define-vars-with-timeout stx)
  (syntax-parse stx
    [(_ [var:id e:expr] ...)
     #'(begin
         (define var git-timeout-flag)
         ...
         (parameterize ([current-subprocess-custodian-mode 'kill]
                        [current-custodian (make-custodian)])
           (let ()
             (define threads (list (thread (λ () (set! var e)))
                                   ...))
             (define master-thread (thread (λ () (map thread-wait threads))))
             ;; TODO - the timeout should be configurable
             (sync/timeout 0.25 master-thread)
             (custodian-shutdown-all (current-custodian)))))]))


(define (get-git-root dir)
  (cond [(directory-exists? (build-path dir ".git")) dir]
        [else (let-values ([(parent end-path must-be-dir) (split-path dir)])
                (if (path? parent)
                    (get-git-root parent)
                    #f))]))

(define (get-current-git-branch)
  #{git branch | fgrep '"*" | sed '"s/*//"})

(define (git-behind/ahead-numbers)
  (define (2char-count a b port)
    (define n-a 0)
    (define n-b 0)
    (for ([c (in-port read-char port)])
      (cond [(eq? c a) (set! n-a (add1 n-a))]
            [(eq? c b) (set! n-b (add1 n-b))]
            [else (void)]))
    (list n-a n-b))
  (with-handlers ([(λ (e) #t) (λ (e) (list 0 0))])
    #{git rev-list --left-right '"@{u}...HEAD" |> 2char-count #\< #\>}))

(define (git-dirty?)
  (define pline #{git diff --quiet --ignore-submodules HEAD &pipeline-ret})
  (not (pipeline-success? pline)))

(define (git-submodule-dirty?)
  (not (equal? "" #{git submodule summary -n 1})))

(define (git-has-untracked?)
  (not (equal? "" #{git ls-files --other --directory --exclude-standard})))

(define (get-git-info)
  (define root (get-git-root (current-directory)))
  (if (not root)
      #f
      (let ()
        (define-vars-with-timeout
          [current-branch (get-current-git-branch)]
          [behind-ahead (git-behind/ahead-numbers)]
          [dirty? (git-dirty?)]
          [sub-dirty? (git-submodule-dirty?)]
          [untracked? (git-has-untracked?)])
        (hash 'root root
              'branch current-branch
              'behind (and (list? behind-ahead)
                           (car behind-ahead))
              'ahead (and (list? behind-ahead)
                          (cadr behind-ahead))
              'dirty? dirty?
              'submodule-dirty? sub-dirty?
              'untracked? untracked?
              'timeout? (for/or ([v (list current-branch behind-ahead dirty?
                                          sub-dirty? untracked?)])
                          (eq? git-timeout-flag v))
              ))))
