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

(require readline)
(require readline/readline)

(define real-stdin pre-readline-input-port)
(define readline-stdin (current-input-port))

(define-line-macro run-pipeline/ret-obj
  (syntax-parser [(_ arg ...)
                  ;; Apparently splicing-syntax-parameterize is leaking through here somehow, and wrapping with a let protects this.
                  #'(let ()
                      (syntax-parameterize ([default-line-macro #'run-pipeline])
                        (run-pipeline &pipeline-ret arg ...)))]))
(define-line-macro run-pipeline/logic/ret-obj
  (syntax-parser [(_ arg ...)
                  #'(let ()
                      (syntax-parameterize ([default-line-macro #'run-pipeline/logic])
                        (run-pipeline/logic &pipeline-ret arg ...)))]))

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
                       (linea-read-syntax (object-name readline-stdin)
                                          readline-stdin))]
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
      (hash-set! (eval 'interactive-return-values repl-namespace)
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
      #`(splicing-with-rash-parameters
         #:in real-stdin
         #:out (current-output-port)
         #:err (current-error-port)
         #:line-macro run-pipeline/logic/ret-obj
         #:starter repl-default-pipeline-starter
         #,@(if splice
                stx
                (list stx)))))))

(define (eval-rashrc.rkt rcfile)
  (eval-syntax (parameterize ([current-namespace repl-namespace])
                 (namespace-syntax-introduce
                  (datum->syntax #f `(require (file ,(path->string rcfile))))))))

(define (eval-rashrc rcfile)
  (repl-eval #:splice #t
             (port->list (λ (p) (linea-read-syntax (object-name p) p))
                         (open-input-file rcfile))))

(define (main)
  ;; Hmm... probably only one of these should count?
  ;(port-count-lines! real-stdin)
  (port-count-lines! readline-stdin)
  (putenv "SHELL" "rash-repl")

  (current-namespace repl-namespace)
  (set-completion-function! composite-complete)

  ;; make real-stdin available to repl
  (eval-syntax (namespace-syntax-introduce
                (datum->syntax #f (list 'define 'real-stdin #'real-stdin))))

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
