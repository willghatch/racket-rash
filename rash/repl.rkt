#lang racket/base

(require
 "main.rkt"
 (submod "private/lang-funcs.rkt" for-repl)
 linea/line-macro
 linea/read
 "private/option-app.rkt"
 "private/rashrc-lib.rkt"
 racket/splicing
 racket/stxparam
 racket/port

 basedir
 racket/exn

 racket/cmdline
 racket/match

 expeditor
 (only-in expeditor/private/param
          current-expeditor-completer
          ee-common-identifiers)
 (only-in syntax-color/racket-lexer racket-lexer)
 (only-in "private/drracket-submit-predicate.rkt" submit-predicate)


 (for-syntax
  racket/base
  syntax/parse
  ))

(define repl-namespace (make-base-namespace))
(current-namespace repl-namespace)
(eval '(require rash
                (only-in rash/private/rashrc-lib
                         current-prompt-function
                         current-rash-top-level-print-formatter)
                rash/private/repl-namespace
                (submod rash/private/top-level-print default-rash-formatter)
                rash/private/help-line-macro
                rash/private/repl-startup-hint
                (for-syntax racket/base syntax/parse))
      repl-namespace)
(define prompt-f (namespace-variable-value 'current-prompt-function))
(define ns-default-rash-formatter
  (parameterize ([current-namespace repl-namespace])
    (namespace-variable-value 'default-rash-formatter)))
(current-rash-top-level-print-formatter ns-default-rash-formatter)

(define interactive-return-values (make-hash))
(define (result-n n)
  (hash-ref interactive-return-values n))
(namespace-set-variable-value! 'result-n result-n)


(define save-readline-history! #f)
(define (cleanup!)
  ;; TODO - I should keep a list of background jobs and send them sighup.
  ;;      - This requires signal abilities in the pipeline library.
  ;;      - As it is, if there is a job running when the shell is killed
  ;;        (eg. by closing a terminal), then it keeps running to completion
  ;;        unless it independently detects eg. its controlling terminal is dead.
  (when save-readline-history!
    (save-readline-history!)))
(define (clean/exit)
  (cleanup!)
  (exit))


(define (rash-repl last-ret-val n last-command-duration reader)
  (with-handlers ([exn:break:hang-up? (λ (e) (clean/exit))]
                  [exn:break:terminate? (λ (e) (clean/exit))]
                  [(λ _ #t) (λ (e) (eprintf "error in prompt function: ~a\n" e))])
    (option-app (prompt-f)
                #:last-return-value last-ret-val
                #:last-return-index n
                #:last-command-duration last-command-duration))
  (flush-output (current-output-port))
  (flush-output (current-error-port))
  (set-box! cwd-hack-box (current-directory))
  (let* ([next-input (with-handlers ([exn:break:hang-up? (λ (e) (clean/exit))]
                                     [exn:break:terminate? (λ (e) (clean/exit))]
                                     [exn? (λ (e) (eprintf "~a\n" e)
                                              #`(void))])
                       (reader))]
         [exit? (if (equal? next-input eof) (clean/exit) #f)]
         [start-time (current-milliseconds)])
    (let* ([ret-val-list
            (call-with-values
             (λ () (with-handlers ([exn:break:hang-up? (λ (e) (clean/exit))]
                                   [exn:break:terminate? (λ (e) (clean/exit))]
                                   [(λ (e) #t) (λ (e) e)])
                     (repl-eval next-input)))
             list)]
           [command-duration (- (current-milliseconds) start-time)]
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
      (rash-repl ret-val new-n command-duration reader))))

(define (repl-eval stx #:splice [splice #f])
  (eval-syntax
   (parameterize ([current-namespace repl-namespace])
     (namespace-syntax-introduce
      #`(splicing-with-rash-config
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
  (define fport (open-input-file rcfile))
  (port-count-lines! fport)
  (define stxs (port->list (λ (p) (linea-read-syntax (object-name p) p)) fport))
  (close-input-port fport)
  (if (null? stxs)
      (void)
      (repl-eval #:splice #t stxs)))

(define (main)
  (port-count-lines! (current-input-port))
  (putenv "SHELL" "rash-repl")

  (current-namespace repl-namespace)

  (define (eval-rash-file eval-fun fname)
    (for ([rcfile (list-config-files #:program "rash" fname)])
      (with-handlers ([(λ _ #t) (λ (ex)
                                  (eprintf "error in rc file ~a: ~a"
                                           rcfile (exn->string ex)))])
        (eval-fun rcfile))))

  (eval-rash-file eval-rashrc.rkt "rashrc.rkt")
  (eval-rash-file eval-rashrc "rashrc")
  (eval '(repl-display-startup-hint))

  ;; TODO:
  ;; 1. persistent history?
  ;; 2. prompt-function
  (call-with-expeditor
   (lambda (reader)
     (current-expeditor-reader
      (lambda (input-port)
        (linea-read-syntax (object-name input-port) input-port)))
     (current-expeditor-ready-checker
      (lambda (input-port) (submit-predicate input-port #f)))
     (current-expeditor-lexer racket-lexer)
     (current-expeditor-color-enabled #t)
     (parameterize ([current-expeditor-completer
                     (lambda (prefix)
                       (values (map string->symbol
                                    ((make-composite-completer complete-paths
                                                               complete-commands
                                                               complete-namespaced) prefix))
                               (ee-common-identifiers)))
                     ])
       (rash-repl (void) 0 0 reader))))

  (printf "and now exiting for some reason\n")
  (clean/exit))

(module+ main
  (main))
