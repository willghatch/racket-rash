#lang racket/base

(require rash)
(require (prefix-in scribble: scribble/reader))
(require rash/private/read-funcs)
(require basedir)
(require
 (for-syntax syntax/parse
             racket/base))

(define-namespace-anchor ns-a)
(define ns (namespace-anchor->namespace ns-a))

(define (rash-repl prev-lines-str last-ret-val)
  (printf "~a > " last-ret-val)
  (let* ([next-line (read-line)]
         [exit? (if (equal? next-line eof) (exit) #f)]
         [input (string-append prev-lines-str next-line)]
         [read-input (with-handlers ([(λ (ex) #t) (λ (ex) 'retry-line)])
                       ;; TODO - I really only want to keep adding lines if
                       ;; the exeption is that it needs a ) to close a (...
                       (scribble:read-inside (open-input-string input)))]
         )
    (if (equal? read-input 'retry-line)
        (rash-repl input "line-not-finished")
        (let ([ret-val
               (with-handlers ([(λ (e) #t) (λ (e) e)])
                 (eval `(rash-line-parse
                         ,@(rash-parse-at-reader-output read-input))
                       ns))])
          (rash-repl "" ret-val)))))

(define (eval-rashrc rcfile)
  (eval `(rash-line-parse ,@(rash-parse-at-reader-output
                             (scribble:read-inside (open-input-file rcfile))))
        ns))

(for ([rcfile (list-config-files #:program "rash" "rashrc")])
  (with-handlers ([(λ _ #t) (λ (ex)
                              (eprintf "error in rc file ~a: ~a" rcfile ex))])
    (eval-rashrc rcfile)))

(with-handlers ([(λ _ #t) (λ (ex)
                            (eprintf "Exception: ~a~n" ex)
                            (rash-repl "" #f))])
  (rash-repl "" #f))

(printf "and now exiting for some reason~n")
