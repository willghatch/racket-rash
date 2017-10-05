#lang racket/base

(require
 "main.rkt"
 (submod "private/lang-funcs.rkt" for-repl)
 "private/repl-namespace.rkt"
 linea/line-macro
 linea/read
 linea/line-parse
 "private/option-app.rkt"
 "private/rashrc-lib.rkt"
 racket/splicing
 shell/pipeline-macro

 basedir
 racket/exn

 (for-syntax
  racket/base
  syntax/parse
  ))

(define-line-macro run-pipeline/ret-obj
  (syntax-parser [(_ arg ...) #'(run-pipeline &pipeline-ret arg ...)]))

(define (rash-repl last-ret-val n)
  (with-handlers ([(λ _ #t) (λ (e) (eprintf "error in prompt function: ~a~n" e))])
    (option-app (current-prompt-function)
                #:last-return-value last-ret-val
                #:last-return-index n))
  (flush-output (current-output-port))
  (flush-output (current-error-port))
  (let* ([next-input (with-handlers ([exn? (λ (e) (eprintf "~a~n" e)
                                              #`(%%linea-racket-line (void)))])
                       (linea-read-syntax (object-name (current-input-port))
                                          (current-input-port)))]
         [exit? (if (equal? next-input eof) (exit) #f)])
    (let* ([ret-val-list
            (call-with-values
             (λ () (with-handlers ([(λ (e) #t) (λ (e) e)])
                     (eval-syntax (parameterize ([current-namespace repl-namespace])
                                    (namespace-syntax-introduce
                                     #`(rash-set-defaults
                                        ((current-input-port)
                                         (current-output-port)
                                         (current-error-port))
                                        (splicing-syntax-parameterize
                                            ([default-line-macro #'run-pipeline/ret-obj]
                                             ;; TODO - make configurable
                                             [default-pipeline-starter
                                               #'repl-default-pipeline-starter])
                                          (linea-line-parse #,next-input))))))))
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
                  (datum->syntax #f `(require (file ,(path->string rcfile))))))))
(define (eval-rashrc rcfile)
  (eval-syntax (parameterize ([current-namespace repl-namespace])
                 (namespace-syntax-introduce
                  #`(rash-set-defaults
                     ((current-input-port)
                      (current-output-port)
                      (current-error-port))
                     (splicing-syntax-parameterize
                         ([default-line-macro #'run-pipeline/ret-obj])
                       (linea-line-parse
                        #,@(linea-read-syntax-all (object-name rcfile)
                                                  (open-input-file rcfile)))))))))

(define (main)
  (port-count-lines! (current-input-port))
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

  (printf "and now exiting for some reason~n"))

(main)
