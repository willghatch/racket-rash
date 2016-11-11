#lang racket/base

(require
 "main.rkt"
 (submod "private/lang-funcs.rkt" for-module-begin)
 "private/repl-namespace.rkt"
 "private/read-funcs.rkt"

 basedir
 racket/exn
 racket/date

 (for-syntax syntax/parse
             racket/base))



(define (format-ret last-ret)
  (cond [(exn? last-ret)
         (begin (eprintf "~a~n" last-ret)
                ;; let any filtering output finish
                (sleep 0.01)
                (red "exn!"))]
        [(pipeline? last-ret)
         ({if (pipeline-success? last-ret)
              green
              red}
          ;(format-ret (pipeline-status last-ret))
          (format "~s" last-ret)
          )]
        [else (format "~s" last-ret)]))

(define (mstyle n)
  (λ (s) (format "\033[~am~a" n s)))
(define (mstyle2 n1 n2)
  (λ (s) (format "\033[~a;~am~a" n1 n2 s)))
(define default-style (mstyle 0))
(define cyan (mstyle 36))
(define red (mstyle 31))
(define green (mstyle 32))
(define bblue (mstyle2 1 34))

(define (basic-prompt last-ret last-ret-n)
  (let* ([cdate (current-date)]
         [chour (date-hour cdate)]
         [cmin (date-minute cdate)]
         [padded-min (if (< cmin 10)
                         (string-append "0" (number->string cmin))
                         cmin)]
         [ret-show (green (format-ret last-ret))])
    (printf "~a:~a ~a ~a~a ~a~a~n~a "
            (cyan chour) padded-min
            (bblue (path->string (current-directory)))
            (default-style "｢R") last-ret-n ret-show (default-style "｣")
            (default-style (if (equal? (system-type 'os) 'windows) ">" "➤")))))
(define current-prompt-function (make-parameter basic-prompt))

(define (rash-repl last-ret-val n)
  ((current-prompt-function) last-ret-val n)
  (flush-output (current-output-port))
  (flush-output (current-error-port))
  (let* ([next-input (with-handlers ([exn? (λ (e) (eprintf "~a~n" e)
                                              #`(%%rash-racket-line (void)))])
                       (rash-read-syntax #f (current-input-port)))]
         [exit? (if (equal? next-input eof) (exit) #f)])
    (let* ([ret-val-list
            (call-with-values
             (λ () (with-handlers ([(λ (e) #t) (λ (e) e)])
                     (eval-syntax (parameterize ([current-namespace repl-namespace])
                                    (namespace-syntax-introduce #`(rash-line-parse
                                                                   #,next-input))))))
             list)]
           [ret-val (if (equal? (length ret-val-list)
                                1)
                        (car ret-val-list)
                        ret-val-list)]
           [new-n (add1 n)])
      (hash-set! (eval 'interactive-return-values repl-namespace)
                 new-n
                 ret-val)
      ;; Sleep just long enough to give any filter ports (eg a highlighted stderr)
      ;; to be able to output before the next prompt.
      (sleep 0.01)
      (rash-repl ret-val new-n))))

(define (eval-rashrc.rkt rcfile)
  (eval-syntax (parameterize ([current-namespace repl-namespace])
                 (namespace-syntax-introduce
                  #`(require (file #,(path->string rcfile)))))))
(define (eval-rashrc rcfile)
  (eval-syntax (parameterize ([current-namespace repl-namespace])
                 (namespace-syntax-introduce
                  #`(rash-line-parse
                     #,@(rash-read-syntax-all (object-name rcfile)
                                              (open-input-file rcfile)))))))

(for ([rcfile (list-config-files #:program "rash" "rashrc.rkt")])
  (with-handlers ([(λ _ #t) (λ (ex)
                              (eprintf "error in rc file ~a: ~a"
                                       rcfile (exn->string ex)))])
    (eval-rashrc.rkt rcfile)))
(for ([rcfile (list-config-files #:program "rash" "rashrc")])
  (with-handlers ([(λ _ #t) (λ (ex)
                              (eprintf "error in rc file ~a: ~a"
                                       rcfile (exn->string ex)))])
    (eval-rashrc rcfile)))

(rash-repl (void) 0)

(printf "and now exiting for some reason~n")
