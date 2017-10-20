#lang racket/base

(provide

 get-git-info

 get-git-root
 get-current-git-branch
 git-behind/ahead-numbers
 git-dirty?
 git-submodule-dirty?
 git-has-untracked?
 )

(require shell/pipeline
         racket/string
         racket/port
         racket/list
         )

(define (get-git-root dir)
  (cond [(directory-exists? (build-path dir ".git")) dir]
        [else (let-values ([(parent end-path must-be-dir) (split-path dir)])
                (if (path? parent)
                    (get-git-root parent)
                    #f))]))

(define (get-current-git-branch)
  (run-pipeline/out '(git branch)
                    (list (λ ()
                            (let ([bs (filter (λ (l) (string-prefix? l "*"))
                                              (port->lines))])
                              (if (null? bs)
                                  (void)
                                  (display (substring (car bs) 2))))))))

(define (git-behind/ahead-numbers)
  (with-handlers ([exn? (λ _ (values 0 0))])
    (let* ([lrs
            (run-pipeline/out '(git rev-list --left-right "@{u}...HEAD")
                              (list (λ ()
                                      (for ([l (port->lines)])
                                        (display (string-ref l 0))))))]
           [lr-list (string->list lrs)])
      ;; < is behind > is ahead
      (values (count (λ (c) (equal? #\< c)) lr-list)
              (count (λ (c) (equal? #\> c)) lr-list)))))

(define (git-dirty?)
  (let ([pline (run-pipeline '(git diff --quiet --ignore-submodules HEAD)
                             #:in 'null
                             #:out 'null
                             #:err 'null
                             )])
    (not (pipeline-success? pline))))

(define (git-submodule-dirty?)
  (pipeline-status (run-pipeline '(git submodule summary -n 1)
                                 (list (λ () (not (equal? "" (port->string)))))
                                 #:in 'null)))

(define (git-has-untracked?)
  (let ([out (run-pipeline/out '(git ls-files --other --directory --exclude-standard))])
    (not (equal? out ""))))

(define (get-git-info)
  (define root (get-git-root (current-directory)))
  (if (not root)
      #f
      (let ()
        (define-values (behind ahead) (git-behind/ahead-numbers))
        (define dirty? (git-dirty?))
        (define sub-dirty? (git-submodule-dirty?))
        (hash 'root root
              'branch (get-current-git-branch)
              'behind behind
              'ahead ahead
              'dirty? (git-dirty?)
              'submodule-dirty? (git-submodule-dirty?)
              'untracked? (git-has-untracked?)
              ))))
