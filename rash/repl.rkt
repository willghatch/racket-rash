#lang racket/base

(require
 rash
 rash/private/read-funcs

 basedir
 racket/exn
 racket/date

 (for-syntax syntax/parse
             racket/base))

(define-namespace-anchor ns-a)
(define ns (namespace-anchor->namespace ns-a))


(define (basic-prompt last-ret)
  (let* ([cdate (current-date)]
         [chour (date-hour cdate)]
         [cmin (date-minute cdate)]
         [padded-min (if (< cmin 10)
                         (string-append "0" (number->string cmin))
                         cmin)]
         [ret-show (if (exn? last-ret)
                       (begin (eprintf "~a~n" last-ret)
                              ;; let any filtering output finish
                              (sleep 0)
                              "exn!")
                       last-ret)])
    (printf "~a:~a ~a~n｢~a｣ ~a "
            chour padded-min
            (path->string (current-directory))
            ret-show
            (if (equal? (system-type 'os) 'windows) ">" "➤"))))
(define current-prompt-function (make-parameter basic-prompt))

(define (rash-repl last-ret-val)
  ((current-prompt-function) last-ret-val)
  (let* ([next-line (read-line)]
         [exit? (if (equal? next-line eof) (exit) #f)]
         [read-input (with-handlers ([(λ (ex) #t)
                                      (λ (ex) (eprintf "~a~n" ex))
                                      #;(λ (ex) 'cantparse)])
                       ;; TODO - I really only want to keep adding lines if
                       ;; the exeption is that it needs a ) to close a (...
                       (rash-read* (open-input-string next-line)))]
         )
    (if (equal? read-input 'cantparse)
        (rash-repl "Couldn't parse input.")
        (let ([ret-val
               (with-handlers ([(λ (e) #t) (λ (e) e)])
                 (eval `(rash-line-parse
                         ,@read-input)
                       ns))])
          ;; Sleep just long enough to give any filter ports (eg a highlighted stderr)
          ;; to be able to output before the next prompt.
          (sleep 0.01)
          (rash-repl ret-val)))))

(define (eval-rashrc rcfile)
  (eval `(rash-line-parse ,@(rash-read-syntax* (object-name rcfile)
                                               (open-input-file rcfile)))
        ns))

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
