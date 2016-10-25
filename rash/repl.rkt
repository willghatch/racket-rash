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
                (sleep 0)
                "exn!")]
        [(pipeline? last-ret)
         (format-ret (pipeline-status last-ret))]
        [(void? last-ret) ""]
        [else (format "~s" last-ret)]))

(define (basic-prompt last-ret)
  (let* ([cdate (current-date)]
         [chour (date-hour cdate)]
         [cmin (date-minute cdate)]
         [padded-min (if (< cmin 10)
                         (string-append "0" (number->string cmin))
                         cmin)]
         [ret-show (format-ret last-ret)])
    (printf "~a:~a ~a~n｢~a｣ ~a "
            chour padded-min
            (path->string (current-directory))
            ret-show
            (if (equal? (system-type 'os) 'windows) ">" "➤"))))
(define current-prompt-function (make-parameter basic-prompt))

(define (rash-repl last-ret-val)
  ((current-prompt-function) last-ret-val)
  (flush-output (current-output-port))
  (flush-output (current-error-port))
  (let* ([next-input (with-handlers ([exn? (λ (e) (eprintf "~a~n" e)
                                              #`(%%rash-racket-line (void)))])
                       (rash-read (current-input-port)))]
         [exit? (if (equal? next-input eof) (exit) #f)])
    (let ([ret-val
           (with-handlers ([(λ (e) #t) (λ (e) e)])
             (eval `(rash-line-parse
                     ,next-input)
                   repl-namespace))])
      ;; Sleep just long enough to give any filter ports (eg a highlighted stderr)
      ;; to be able to output before the next prompt.
      (sleep 0.01)
      (rash-repl ret-val))))

(define (eval-rashrc rcfile)
  (eval `(rash-line-parse ,@(rash-read-syntax-all (object-name rcfile)
                                                  (open-input-file rcfile)))
        repl-namespace))

(for ([rcfile (list-config-files #:program "rash" "rashrc")])
  (with-handlers ([(λ _ #t) (λ (ex)
                              (eprintf "error in rc file ~a: ~a"
                                       rcfile (exn->string ex)))])
    (eval-rashrc rcfile)))

(with-handlers ([(λ _ #t) (λ (ex)
                            (eprintf "Exception: ~a~n" (exn->string ex))
                            (rash-repl #f))])
  (rash-repl #f))

(printf "and now exiting for some reason~n")
