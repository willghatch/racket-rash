#lang racket/base

(require rash)
(require (prefix-in scribble: scribble/reader))
(require rash/private/read-funcs)
(require
 (for-syntax syntax/parse
             racket/base))

(define-namespace-anchor ns-a)
(define ns (namespace-anchor->namespace ns-a))

(define (rash-repl prev-lines-str last-ret-val)
  (printf "~a >" last-ret-val)
  (let* ([next-line (read-line)]
         [input (string-append prev-lines-str next-line)]
         [read-input (with-handlers ([(λ (ex) #t) (λ (ex) 'retry-line)])
                       ;; TODO - I really only want to keep adding lines if
                       ;; the exeption is that it needs a ) to close a (...
                       (scribble:read-inside (open-input-string input)))]
         )
    (if (equal? read-input 'retry-line)
        (rash-repl input "line-not-finished")
        (let ([ret-val
               (eval `(rash-line-parse
                       ,@(rash-parse-at-reader-output read-input)) ns)])
          (rash-repl "" ret-val)))))

(with-handlers ([(λ _ #t) (λ (ex)
                            (eprintf "Exception: ~a~n" ex)
                            (rash-repl "" #f))])
  (rash-repl "" #f))
