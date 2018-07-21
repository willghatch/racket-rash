#lang racket/base

(require
 "main.rkt"
 (submod "private/lang-funcs.rkt" for-repl)
 "private/repl-namespace.rkt"
 linea/line-macro
 linea/read
 "private/option-app.rkt"
 "private/rashrc-lib.rkt"
 racket/splicing
 racket/stxparam
 racket/port

 basedir
 racket/exn

 (for-syntax
  racket/base
  syntax/parse
  ))

;(require readline)
;(require readline/readline)

;(define real-stdin pre-readline-input-port)
;(define readline-stdin (current-input-port))

(define repl-namespace (make-base-namespace))
(eval '(require rash
                (only-in rash/private/rashrc-lib
                         current-prompt-function
                         current-rash-top-level-print-formatter)
                (except-in rash/private/repl-namespace
                           interactive-return-values)
                (submod rash/private/top-level-print default-rash-formatter)
                (for-syntax racket/base syntax/parse))
      repl-namespace)
(define ns-default-rash-formatter
  (parameterize ([current-namespace repl-namespace])
    (namespace-variable-value 'default-rash-formatter)))
(current-rash-top-level-print-formatter ns-default-rash-formatter)


(define (clean/exit)
  ;; TODO - I should keep a list of background jobs and send them sighup.
  ;;      - This requires signal abilities in the pipeline library.
  ;;      - As it is, if there is a job running when the shell is killed
  ;;        (eg. by closing a terminal), then it keeps running to completion
  ;;        unless it independently detects eg. its controlling terminal is dead.
  (exit))


(define (rash-repl last-ret-val n)
  (with-handlers ([exn:break:hang-up? (λ (e) (clean/exit))]
                  [exn:break:terminate? (λ (e) (clean/exit))]
                  [(λ _ #t) (λ (e) (eprintf "error in prompt function: ~a\n" e))])
    (option-app (current-prompt-function)
                #:last-return-value last-ret-val
                #:last-return-index n))
  (flush-output (current-output-port))
  (flush-output (current-error-port))
  (set-box! cwd-hack-box (current-directory))
  (let* ([next-input (with-handlers ([exn:break:hang-up? (λ (e) (clean/exit))]
                                     [exn:break:terminate? (λ (e) (clean/exit))]
                                     [exn? (λ (e) (eprintf "~a\n" e)
                                              #`(void))])
                       (linea-read-syntax (object-name (current-input-port))
                                          (current-input-port)))]
         [exit? (if (equal? next-input eof) (exit) #f)])
    (let* ([ret-val-list
            (call-with-values
             (λ () (with-handlers ([exn:break:hang-up? (λ (e) (clean/exit))]
                                   [exn:break:terminate? (λ (e) (clean/exit))]
                                   [(λ (e) #t) (λ (e) e)])
                     (repl-eval next-input)))
             list)]
           [ret-val (if (equal? (length ret-val-list)
                                1)
                        (car ret-val-list)
                        ret-val-list)]
           [new-n (add1 n)])
      (hash-set! interactive-return-values
                 new-n
                 ret-val)
      ;; Sleep just long enough to give any filter ports (eg a highlighted stderr)
      ;; to be able to output before the next prompt.
      (sleep 0.01)
      (rash-repl ret-val new-n))))

(define (repl-eval stx #:splice [splice #f])
  (eval-syntax
   (parameterize ([current-namespace repl-namespace])
     (namespace-syntax-introduce
      #`(splicing-with-rash-config
         ;#:in real-stdin
         #:in (current-input-port)
         #:out (current-output-port)
         #:err (current-error-port)
         #:line-macro repl-default-line-macro
         #:starter repl-default-pipeline-starter
         #,@(if splice
                stx
                (list stx)))))))

(define (eval-rashrc.rkt rcfile)
  (eval-syntax (parameterize ([current-namespace repl-namespace])
                 (namespace-syntax-introduce
                  (datum->syntax #f `(require (file ,(path->string rcfile))))))))

(define (eval-rashrc rcfile)
  (define stxs (port->list (λ (p) (linea-read-syntax (object-name p) p))
                           (open-input-file rcfile)))
  (if (null? stxs)
      (void)
      (repl-eval #:splice #t stxs)))

(define (main)
  ;; Hmm... probably only one of these should count?
  ;(port-count-lines! real-stdin)
  ;(port-count-lines! readline-stdin)
  (port-count-lines! (current-input-port))
  (putenv "SHELL" "rash-repl")

  (current-namespace repl-namespace)

  ;(set-completion-function! composite-complete)

  ;; make real-stdin available to repl
  ;(eval-syntax (namespace-syntax-introduce
  ;              (datum->syntax #f (list 'define 'real-stdin #'real-stdin))))
  (eval '(displayln "hilo") repl-namespace)

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

  (printf "and now exiting for some reason\n"))

(main)
