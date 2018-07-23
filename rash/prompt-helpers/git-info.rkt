#lang rash

(provide
 git-info

 git-root
 git-branch
 git-behind/ahead-numbers
 git-dirty?
 git-submodule-dirty?
 git-has-untracked?
 git-remote-tracking?
 )

(require
 (for-syntax
  racket/base
  syntax/parse
  ))

(define git-timeout-flag (gensym))

(define-syntax (define-vars-with-timeout stx)
  (syntax-parse stx
    [(_ timeout [var:id var-e:expr] ...)
     #'(begin
         (define var git-timeout-flag)
         ...
         (parameterize ([current-subprocess-custodian-mode 'kill]
                        [current-custodian (make-custodian)])
           (let ()
             (define threads (list (thread (λ () (with-handlers
                                                   ([(λ (e) #t) (λ (e) (void))])
                                                   (set! var var-e))))
                                   ...))
             (define master-thread (thread (λ () (map thread-wait threads))))
             ;; TODO - the timeout should be configurable
             (sync/timeout timeout master-thread)
             (custodian-shutdown-all (current-custodian)))))]))


(define (git-root [dir (current-directory)])
  (cond [(directory-exists? (build-path dir ".git")) dir]
        [else (let-values ([(parent end-path must-be-dir) (split-path dir)])
                (if (path? parent)
                    (git-root parent)
                    #f))]))

(define (git-branch [dir (current-directory)])
  (parameterize ([current-directory dir])
    #{git branch | fgrep '"*" | sed '"s/*//"}))

(define (git-behind/ahead-numbers [dir (current-directory)])
  (define (2char-count a b port)
    (define n-a 0)
    (define n-b 0)
    (for ([c (in-port read-char port)])
      (cond [(eq? c a) (set! n-a (add1 n-a))]
            [(eq? c b) (set! n-b (add1 n-b))]
            [else (void)]))
    (list n-a n-b))
  (parameterize ([current-directory dir])
    (with-handlers ([(λ (e) #t) (λ (e) (list 0 0))])
      #{git rev-list --left-right '"@{u}...HEAD" |> 2char-count #\< #\>})))

(define (git-remote-tracking? [dir (current-directory)])
  (define pline
    (parameterize ([current-directory dir])
      #{git rev-parse --abbrev-ref '"@{upstream}" &pipeline-ret}))
  (pipeline-success? pline))

(define (git-dirty? [dir (current-directory)])
  (define pline
    (parameterize ([current-directory dir])
      #{git diff --quiet --ignore-submodules HEAD &pipeline-ret}))
  (not (pipeline-success? pline)))

(define (git-submodule-dirty? [dir (current-directory)])
  (parameterize ([current-directory dir])
    (not (equal? "" #{git submodule summary -n 1}))))

(define (git-has-untracked? [dir (current-directory)])
  (parameterize ([current-directory dir])
    (not (equal? "" #{git ls-files --other --directory --exclude-standard}))))

(define (git-info [dir (current-directory)]
                  #:timeout [timeout 0.25])
  (define root (git-root dir))
  (if (not root)
      #f
      (parameterize ([current-directory dir])
        (let ()
          (define-vars-with-timeout
            timeout
            [current-branch (git-branch)]
            [behind-ahead (git-behind/ahead-numbers)]
            [dirty? (git-dirty?)]
            [sub-dirty? (git-submodule-dirty?)]
            [untracked? (git-has-untracked?)]
            [remote-tracking? (git-remote-tracking?)])
          (for/fold ([h (hash)])
                    ([k-v-list
                      (list (list 'root root)
                            (list 'branch current-branch)
                            (list 'behind (and (list? behind-ahead)
                                               (car behind-ahead)))
                            (list 'ahead (and (list? behind-ahead)
                                              (cadr behind-ahead)))
                            (list 'dirty? dirty?)
                            (list 'submodule-dirty? sub-dirty?)
                            (list 'untracked? untracked?)
                            (list 'remote-tracking? remote-tracking?)
                            (list 'timeout?
                                  (for/or ([v (list current-branch
                                                    behind-ahead dirty?
                                                    sub-dirty?
                                                    untracked?)])
                                    (eq? git-timeout-flag v))))])
            (if (eq? git-timeout-flag (cadr k-v-list))
                h
                (hash-set h (car k-v-list) (cadr k-v-list))))))))
