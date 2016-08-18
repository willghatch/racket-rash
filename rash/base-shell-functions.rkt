#lang racket/base

(require shell/pipeline)
(require racket/exn)
(provide (all-defined-out))

(define (change-current-directory dir)
  (if (directory-exists? dir)
      (current-directory dir)
      (error 'change-current-directory "No such directory: ~a" dir)))

(define (shell-func-catch-error e)
  (eprintf "~a" (exn->string e))
  1)

(define cd
  (pipeline-same-thread-func
   (λ dirs
     (let ([dir (if (null? dirs) (getenv "HOME") (car dirs))]
           [too-many (if (> (length dirs) 1)
                         (error 'cd "too many arguments")
                         #f)])
       (change-current-directory
        (cond [(string? dir) dir]
              [(path? dir) dir]
              [(symbol? dir) (symbol->string dir)]
              [else (error 'cd "cd argument needs to be a string, path, or symbol")])))
     0)))

(define (shell-printf f-string . args)
  (with-handlers ([(λ _ #t) shell-func-catch-error])
    (apply printf f-string args))
  0)

(define (shell-echo . args)
  (for ([a args]
        [i (in-naturals)])
    (when (not (equal? i 0))
      (display " "))
    (display a))
  (display "\n")
  0)

(define base-shell-functions
  (hash "cd" cd
        "printf" shell-printf
        "echo" shell-echo
        ))

(define (use-base-shell-functions)
  (current-shell-functions base-shell-functions))

(module+ main
  (use-base-shell-functions)
  (run-pipeline `(echo hello everybody))
  (run-pipeline `(echo this is a test))
  )
