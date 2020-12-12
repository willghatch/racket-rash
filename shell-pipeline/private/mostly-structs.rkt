#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [path-string-symbol? (-> any/c boolean?)]
  [unix-pipeline-member-spec? (-> any/c boolean?)]
  [object-pipeline-member-spec? (-> any/c boolean?)]
  [composite-pipeline-member-spec? (-> any/c boolean?)]

  [rename mk-unix-pipeline-member-spec unix-pipeline-member-spec
          (->* ((listof any/c))
               (#:err (or/c output-port?
                            false/c
                            path-string-symbol?
                            file-redirection-spec?
                            special-redirect?
                            (list/c path-string-symbol?
                                    (or/c 'error 'append 'truncate))
                            pipeline-default-option?)
                #:success (or/c false/c
                                procedure?
                                (listof any/c)
                                pipeline-default-option?)
                )
               unix-pipeline-member-spec?)]
  [object-pipeline-member-spec (-> procedure? object-pipeline-member-spec?)]
  [composite-pipeline-member-spec (-> (listof (or/c unix-pipeline-member-spec?
                                                    object-pipeline-member-spec?
                                                    composite-pipeline-member-spec?))
                                      composite-pipeline-member-spec?)]
  [rename mk-file-redirection-spec
          file-redirect
          (->* (path-string-symbol?)
               (#:exists (or/c 'error 'append 'truncate))
               file-redirection-spec?)]

  [shell-substitution (-> (-> shell-substitution-hash?) shell-substitution?)]
  )


 special-redirect?
 null-redirect
 stderr-capture-redirect
 shared-stderr-capture-redirect
 stdout-redirect
 stderr-redirect

 shell-substitution?

 ;; These ones are not really for public consumption.
 special-redirect-type
 pipeline-default-option
 pipeline-default-option?
 file-redirection-spec?
 file-redirection-spec-file
 file-redirection-spec-exists-flag

 shell-substitution-thunk
 do-shell-substitution
 shell-substitution-done
 shell-substitution-done?
 shell-substitution-done-argument
 shell-substitution-done-pipeline-done-procedures

 )

(module+ internals
  (provide
   (rename-out
    [unix-pipeline-member-spec-argl pipeline-member-spec-argl]
    [unix-pipeline-member-spec-port-err pipeline-member-spec-port-err]
    [unix-pipeline-member-spec-success-pred pipeline-member-spec-success-pred]
    )
   object-pipeline-member-spec-func
   composite-pipeline-member-spec-members
   ))


(struct object-pipeline-member-spec
  (func)
  #:transparent)

(struct composite-pipeline-member-spec
  (members)
  #:transparent)

(struct unix-pipeline-member-spec
  (argl port-err success-pred)
  #:transparent)
(define (mk-unix-pipeline-member-spec argl
                                      #:err [port-err (pipeline-default-option)]
                                      #:success [success-pred (pipeline-default-option)])
  (unix-pipeline-member-spec argl port-err success-pred))

(struct pipeline-default-option ())

(struct file-redirection-spec (file exists-flag))
(define (mk-file-redirection-spec f #:exists [exists 'error])
  (file-redirection-spec f exists))

(define (path-string-symbol? pss)
  (or (path-string? pss)
      (and (symbol? pss) (path-string? (symbol->string pss)))))

(struct special-redirect (type))
;; null-redirect is supported by input, output, and error ports
(define null-redirect (special-redirect 'null))
;; The next ones are only supported by error ports
(define stderr-capture-redirect (special-redirect 'stderr-capture))
(define shared-stderr-capture-redirect (special-redirect 'shared-stderr-capture))
(define stdout-redirect (special-redirect 'stdout))
;; TODO - this one is not supported at all, but it should be (by output ports).
(define stderr-redirect (special-redirect 'stderr))

(struct shell-substitution
  ;; substitution-thunk must return a hash table with keys:
  ;; required:
  ;; 'argument : the string to send to `subprocess`
  ;; optional:
  ;; 'pipeline-done-procedure : a procedure (-> subprocess-pipeline? any/c) that can do cleanup for the substitution, such as delete a temporary file, named pipe, etc.
  (thunk))

(define (shell-substitution-hash? v)
  (and (hash? v)
       (hash-has-key? v 'argument)
       (let ([pdp (hash-ref v 'pipeline-done-procedure #f)])
         (or (not pdp) (and (procedure? pdp) (procedure-arity-includes? pdp 1))))))
(struct shell-substitution-done (argument pipeline-done-procedures))
(define (do-shell-substitution substitution)
  (define (rec sub done-procs)
    (define sub-thunk (shell-substitution-thunk sub))
    (define result (sub-thunk))
    (define arg (hash-ref result 'argument))
    (define done-proc (hash-ref result 'pipeline-done-procedure #f))
    (define new-done-procs (if done-proc (cons done-proc done-procs) done-procs))
    (if (shell-substitution? arg)
        (rec arg new-done-procs)
        (shell-substitution-done (hash-ref result 'argument) new-done-procs)))
  (rec substitution '()))

