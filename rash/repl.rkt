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

(define (repl-read input-port)
  (linea-read-syntax (object-name input-port) input-port))

(define (rash-repl last-ret-val n input-port expeditor-read)
  (define read-func
    (if expeditor-read
        expeditor-read
        (λ () (list (repl-read input-port)))))
  (with-handlers ([exn:break:hang-up? (λ (e) (clean/exit))]
                  [exn:break:terminate? (λ (e) (clean/exit))]
                  [(λ _ #t) (λ (e) (eprintf "error in prompt function: ~a\n" e))])
    (option-app (prompt-f)
                #:last-return-value last-ret-val
                #:last-return-index n))
  (flush-output (current-output-port))
  (flush-output (current-error-port))
  (set-box! cwd-hack-box (current-directory))
  (let* ([next-inputs (with-handlers ([exn:break:hang-up? (λ (e) (clean/exit))]
                                      [exn:break:terminate? (λ (e) (clean/exit))]
                                      [exn? (λ (e) (eprintf "~a\n" e)
                                              #`(void))])
                        (read-func))]
         [exit? (if (or (eof-object? next-inputs)
                        (null? next-inputs))
                    (clean/exit)
                    #f)])
    (let* ([eval-1
            (λ (one-stx)
              (call-with-values
               (λ () (with-handlers ([exn:break:hang-up? (λ (e) (clean/exit))]
                                     [exn:break:terminate? (λ (e) (clean/exit))]
                                     [(λ (e) #t) (λ (e) e)])
                       (repl-eval one-stx)))
               list))]
           [ret-val-list-list
            (for/list ([next-input next-inputs])
              (if (eof-object? next-input)
                  (clean/exit)
                  (eval-1 next-input)))]
           [ret-val-list (map (λ (ret-val-list)
                                (if (equal? (length ret-val-list)
                                            1)
                                    (car ret-val-list)
                                    ret-val-list))
                              ret-val-list-list)]
           [ret-val (if (equal? (length ret-val-list) 1)
                        (car ret-val-list)
                        ret-val-list)]
           [new-n (add1 n)])
      (hash-set! interactive-return-values
                 new-n
                 ret-val)
      ;; Sleep just long enough to give any filter ports (eg a highlighted stderr)
      ;; to be able to output before the next prompt.
      (sleep 0.01)
      (rash-repl ret-val new-n input-port expeditor-read))))

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
  (define use-readline? (not (equal? (system-type 'os) 'windows)))
  (define readline-persistent-history? #t)
  (define (cmdline-bool->bool b)
    (match (string-downcase b)
      ["true" #t]
      ["false" #f]
      [else (error
             'command-line
             "--readline flag requires literal \"true\" or \"false\" strings")]
      ))

  (command-line
   #:program "rash-repl"
   #:once-each
   ["--readline" readline?
                 "use readline (true or false)"
                 (set! use-readline? (cmdline-bool->bool readline?))]
   ["--readline-persistent-history" persistent-history?
                 "persist history for readline (true or false)"
                 (set! readline-persistent-history?
                       (cmdline-bool->bool persistent-history?))]
   )

  (define input-port-for-repl (current-input-port))
  (when use-readline?
    (let ()
      (define pre-readline-input-port
        (dynamic-require 'readline 'pre-readline-input-port
                         (λ () (error 'readline-didnt-require-right))))
      (set! input-port-for-repl (current-input-port))
      (current-input-port pre-readline-input-port)

      (define set-completion-function!
        (dynamic-require 'readline/readline 'set-completion-function!))
      (set-completion-function! (make-composite-completer complete-paths
                                                          ;complete-commands
                                                          ;complete-namespaced
                                                          ))
      (prompt-f
       (dynamic-require 'rash/private/rashrc-lib 'basic-prompt))

      ;;;; Input History
      ;; The readline library automatically loads the history from the racket-prefs
      ;; file, meaning we get the normal racket repl history.
      ;; It also saves the old version of the history on exit to that same file
      ;; (after potentially saving the new history as well...).
      ;; So we will make some effort to manage the history manually.
      ;; We will clear the history, load a different history,
      ;; then save the history when exiting.
      (define history-length (dynamic-require 'readline/readline 'history-length))
      (define history-delete (dynamic-require 'readline/readline 'history-delete))
      (define history-get (dynamic-require 'readline/readline 'history-get))
      (define history-add (dynamic-require 'readline/readline 'add-history))
      (define get-preference (dynamic-require 'racket/file 'get-preference))
      (define put-preferences (dynamic-require 'racket/file 'put-preferences))
      (when readline-persistent-history?
        (for ([i (in-range (history-length))])
          (history-delete 0))
        (define rash-readline-input-history
          (get-preference 'rash:repl:readline-input-history (λ () null)))
        (for ([h rash-readline-input-history])
          (history-add h))
        (set! save-readline-history!
              (λ ()
                ;; TODO - I should trim this to a maximum length.
                (define rash-history
                  (for/list ([i (in-range (history-length))])
                    (history-get i)))
                (put-preferences '(rash:repl:readline-input-history)
                                 (list rash-history)))))))

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

  (rash-repl (void) 0 input-port-for-repl #f)

  (printf "and now exiting for some reason\n")
  (clean/exit))

(module+ main
  (main))
(module+ _out-for-expeditor
  (provide rash-repl repl-read))
