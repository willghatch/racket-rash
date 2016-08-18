#lang racket/base

(require racket/port)
(require racket/exn)
(require racket/list)

(provide
 shellify
 (struct-out alias-func)
 current-shell-functions

 run-pipeline
 run-pipeline/funcify
 (struct-out pipeline-member-spec)

 pipeline-err-ports
 pipeline-wait
 pipeline-kill
 pipeline-status
 pipeline-status/end
 pipeline-status/all
 pipeline-status/list
 )

;; TODO -- contracts

(define current-shell-functions
  (make-parameter (hash)))

(define (lookup-shell-function key)
  (let* ([skey (->string key)])
    (hash-ref (current-shell-functions) skey #f)))

(define (resolve-alias pm-spec)
  (if (pm-spec-alias? pm-spec)
      (let* ([old-argl (pipeline-member-spec-argl pm-spec)]
             [new-argl (apply (car old-argl) (cdr old-argl))]
             [error? (if (or (not (list? new-argl)) (null? new-argl))
                         (error 'resolve-alias "alias did not produce an argument list")
                         #f)])
        (struct-copy pipeline-member-spec pm-spec
                     [argl new-argl]))
      pm-spec))

(define (resolve-pipeline-member-spec-path pm-spec)
  (let* ([argl (pipeline-member-spec-argl pm-spec)]
         [cmd (car argl)]
         [cmdpath (resolve-command-path cmd)])
    (if cmdpath
        (struct-copy pipeline-member-spec pm-spec
                     [argl (cons cmdpath (cdr argl))])
        (error 'resolve-pipeline-member-spec-path
               "Command not found: ~a" cmd))))

(define (resolve-command-path cmd)
  (let ([pathstr (if (path? cmd) cmd (->string cmd))])
    (or (find-executable-path pathstr)
        (and (equal? 'windows (system-type 'os))
             (string? pathstr)
             (find-executable-path
              (string-append (->string cmd) ".exe"))))))

(define (symstr? x)
  (or (symbol? x) (string? x)))

(define (resolve-spec pm-spec)
  (let* ([argl (pipeline-member-spec-argl pm-spec)]
         [cmd (first argl)]
         [cmdstr (and (symstr? cmd) (->string cmd))])
    (cond
      [(pm-spec-alias? pm-spec) (resolve-spec (resolve-alias pm-spec))]
      [(equal? cmdstr "process") (resolve-pipeline-member-spec-path
                                  (struct-copy pipeline-member-spec pm-spec
                                               [argl (cdr argl)]))]
      [cmdstr
       (let ([looked (lookup-shell-function cmdstr)])
         (if looked
             (resolve-spec (struct-copy pipeline-member-spec pm-spec
                                        [argl (cons looked (cdr argl))]))
             (resolve-pipeline-member-spec-path pm-spec)))]
      [else pm-spec])))

;;;; Pipeline Members ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct pipeline-member
  (subproc-or-thread port-err thread-ret-box thread-exn-box)
  #:transparent)
(struct pipeline-member-spec
  (argl port-err)
  ;; TODO - add contracts
  #:transparent)
(struct alias-func
  (func)
  #:property prop:procedure (struct-field-index func)
  #:transparent)

(define (pm-spec-lookup? pmember)
  (and (pipeline-member-spec? pmember)
       (let ([cmd (car (pipeline-member-spec-argl pmember))])
         (or (symbol? cmd) (string? cmd)))))
(define (pm-spec-path? pmember)
  (and (pipeline-member-spec? pmember)
       (path? (car (pipeline-member-spec-argl pmember)))))
(define (pm-spec-func? pmember)
  (and (pipeline-member-spec? pmember)
       (not (pm-spec-alias? pmember))
       (procedure? (car (pipeline-member-spec-argl pmember)))))
(define (pm-spec-alias? pmember)
  (and (pipeline-member-spec? pmember)
       (alias-func? (car (pipeline-member-spec-argl pmember)))))

(define (pipeline-member-process? pmember)
  (and (pipeline-member? pmember)
       (subprocess? (pipeline-member-subproc-or-thread pmember))))
(define (pipeline-member-thread? pmember)
  (and (pipeline-member? pmember)
       (thread? (pipeline-member-subproc-or-thread pmember))))

(define (pipeline-member-wait member)
  (if (pipeline-member-process? member)
      (subprocess-wait (pipeline-member-subproc-or-thread member))
      (thread-wait (pipeline-member-subproc-or-thread member))))

(define (pipeline-member-kill m)
  (if (pipeline-member-process? m)
      (subprocess-kill (pipeline-member-subproc-or-thread m) #t)
      (kill-thread (pipeline-member-subproc-or-thread m))))

(define (pipeline-member-status m)
  (if (pipeline-member-process? m)
      (subprocess-status (pipeline-member-subproc-or-thread m))
      (let* ([dead (thread-dead? (pipeline-member-subproc-or-thread m))]
             [ret (unbox (pipeline-member-thread-ret-box m))]
             [err (unbox (pipeline-member-thread-exn-box m))])
        (if (not dead)
            'running
            (or ret err)))))

;;;; Pipelines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct pipeline
  (port-to port-from members end-exit-flag start-bg? status-all?)
  #:transparent)

(define (pipeline-spec? pline)
  (for/and ([m (pipeline-members pline)])
    (pipeline-member-spec? m)))

(define (pipeline-err-ports pline)
  (map pipeline-member-port-err (pipeline-members pline)))

(define (pipeline-wait/all pline)
  (for ([m (pipeline-members pline)])
    (pipeline-member-wait m)))
(define (pipeline-wait/end pline)
  (pipeline-member-wait (car (reverse (pipeline-members pline)))))
(define (pipeline-wait pline)
  (if (pipeline-status-all? pline)
      (pipeline-wait/all pline)
      (pipeline-wait/end pline)))

(define (pipeline-kill pline)
  (for ([m (pipeline-members pline)])
    (pipeline-member-kill m)))

#|
TODO - pipeline status should have a special value for pipeline
members that have been killed.  Also, it should count as success for
pipelines where it is set to always kill when the end member exits
|#
(define (pipeline-status/list pline)
  (map pipeline-member-status (pipeline-members pline)))
(define (pipeline-status/all pline)
  (define (rec sl)
    (cond [(null? sl) 0]
          [(equal? 0 (car sl)) (rec (cdr sl))]
          [else (car sl)]))
  (rec (pipeline-status/list pline)))
(define (pipeline-status/end pline)
  (pipeline-member-status (car (reverse (pipeline-members pline)))))
(define (pipeline-status pline)
  (if (pipeline-status-all? pline)
      (pipeline-status/all pline)
      (pipeline-status/end pline)))


(define (make-pipeline-spec #:in [in (current-input-port)]
                            #:out [out (current-output-port)]
                            #:end-exit-flag [end-exit-flag #t]
                            #:status-all? [status-all? #f]
                            #:background? [bg? #f]
                            #:default-err [default-err (current-error-port)]
                            members)
  (pipeline in out
            (map (λ (m)
                   (if (pipeline-member-spec? m)
                       m
                       (pipeline-member-spec m default-err)))
                 members)
            end-exit-flag bg? status-all?))

(define (run-pipeline #:in [in (current-input-port)]
                      #:out [out (current-output-port)]
                      #:end-exit-flag [end-exit-flag #t]
                      #:status-all? [status-all? #f]
                      #:background? [bg? #f]
                      #:default-err [default-err (current-error-port)]
                      . members)
  (let ([pline
         (run-pipeline/spec
          (make-pipeline-spec #:in in #:out out
                              #:end-exit-flag end-exit-flag
                              #:status-all? status-all?
                              #:background? bg?
                              #:default-err default-err
                              members))])
    (if bg?
        pline
        (begin (pipeline-wait pline)
               (pipeline-status pline)))))

(define (run-pipeline/funcify #:end-exit-flag [end-exit-flag #t]
                              #:status-all? [status-all? #f]
                              . members)
  (let* ([out (open-output-string)]
         [err (open-output-string)]
         [in (open-input-string "")]
         [pline-spec (make-pipeline-spec #:in in #:out out
                                         #:end-exit-flag end-exit-flag
                                         #:status-all? status-all?
                                         #:background? #f
                                         #:default-err err
                                         members)]
         [pline (parameterize ([current-output-port out]
                               [current-error-port err]
                               [current-input-port in])
                  (run-pipeline/spec pline-spec))]
         [wait (pipeline-wait pline)]
         [kill (pipeline-kill pline)]
         [status (pipeline-status pline)])
    (if (not (equal? 0 status))
        (error 'rash-pipeline/funcify
               "nonzero pipeline exit (~a) with stderr: ~v"
               status
               (get-output-string err))
        (get-output-string out))))


(define (run-pipeline/spec pipeline-spec)
  (let* ([members-pre-resolve (pipeline-members pipeline-spec)]
         [members (map resolve-spec members-pre-resolve)]
         [kill-flag (pipeline-end-exit-flag pipeline-spec)]
         [status-all? (pipeline-status-all? pipeline-spec)]
         [bg? (pipeline-start-bg? pipeline-spec)]
         [to-port (pipeline-port-to pipeline-spec)]
         [to-use (if (and (pm-spec-path? (car members))
                          (port? to-port)
                          (not (file-stream-port? to-port)))
                     #f
                     to-port)]
         [from-port (pipeline-port-from pipeline-spec)]
         [from-use (if (and (pm-spec-path? (car (reverse members)))
                            (port? from-port)
                            (not (file-stream-port? from-port)))
                       #f
                       from-port)]
         [run-members/ports (run-pipeline-members members to-use from-use)]
         [run-members (first run-members/ports)]
         [to-out (second run-members/ports)]
         [from-out (third run-members/ports)]
         [from-ret (if (equal? from-port from-use)
                       from-out
                       (begin
                         (thread (λ () (copy-port from-out from-port)))
                         #f))]
         [to-ret (if (equal? to-port to-use)
                     to-out
                     (begin
                       (thread (λ () (copy-port to-port to-out)))
                       #f))]
         [pline (pipeline to-ret from-ret run-members kill-flag bg? status-all?)]
         [killer (if (or (equal? kill-flag 'always)
                         (and kill-flag
                              (not status-all?)))
                     (thread (λ () (pipeline-wait pline)
                                (pipeline-kill pline)))
                     #f)]
         [pline-with-killer (struct-copy pipeline pline
                                         [end-exit-flag killer])])
    pline-with-killer))

(define (run-pipeline-members pipeline-member-specs to-line-port from-line-port)
  ;; Start a the pipeline, return started members.
  ;; To-line-port and from-line-port should be file-stream-ports if they connect
  ;; to processes.
  (struct pmi
    (port-to port-from port-err subproc-or-thread thread-ret-box thread-exn-box)
    #:transparent)
  (define (pmi-process? pmember)
    (and (pmi? pmember) (subprocess? (pmi-subproc-or-thread pmember))))
  (define (pmi-thread? pmember)
    (and (pmi? pmember) (thread? (pmi-subproc-or-thread pmember))))

  (define pipeline-length (length pipeline-member-specs))
  (define r1-members-rev
    (for/fold ([m-outs '()])
              ([m pipeline-member-specs]
               [i (in-range pipeline-length)])
      (cond
        ;; leave thread starting to round 2
        [(pm-spec-func? m) (cons m m-outs)]
        [else
         (let* ([to-spec (cond [(null? m-outs)
                                to-line-port]
                               [(pmi-process? (car m-outs))
                                (pmi-port-from (car m-outs))]
                               [else #f])]
                [from-spec (if (equal? i (sub1 pipeline-length))
                               from-line-port
                               #f)]
                [err-spec (pipeline-member-spec-port-err m)]
                [err-to-send (if (and (port? err-spec)
                                      (not (file-stream-port? err-spec)))
                                 #f
                                 err-spec)])
           (let-values ([(sproc from to err-from)
                         (subprocess+ (pipeline-member-spec-argl m)
                                      #:err err-to-send
                                      #:in to-spec
                                      #:out from-spec)])
             (let ([out-member (pmi to from err-from sproc #f #f)])
               (when (and err-spec err-from)
                 ;; I need to wire up the file-stream-port output into the original port
                 (thread (λ () (copy-port err-from err-spec))))
               (cons out-member m-outs))))])))

  (define r1-members (reverse r1-members-rev))
  (define r2-members-rev
    (for/fold ([m-outs '()])
              ([m r1-members]
               [i (in-range pipeline-length)])
      (cond
        [(pm-spec-func? m)
         (let* ([prev (and (not (null? m-outs))
                           (car m-outs))]
                [next (and (< i (sub1 pipeline-length))
                           (list-ref r1-members i))]
                [next-is-process? (and next (pmi-process? next))]
                [err-spec (pipeline-member-spec-port-err m)]
                [ret-box (box #f)]
                [err-box (box #f)])
           (let-values ([(to-use to-ret)
                         (cond
                           [(and (not prev) (not to-line-port)) (make-pipe)]
                           [prev (values (pmi-port-from prev) #f)]
                           [else (values to-line-port #f)])]
                        [(from-ret from-use)
                         (cond
                           [next-is-process? (values #f (pmi-port-to next))]
                           [next (make-pipe)]
                           [(not from-line-port) (make-pipe)]
                           [else (values #f from-line-port)])]
                        [(err-ret err-use)
                         (if err-spec
                             (values #f err-spec)
                             (make-pipe))])
             (let* ([ret-thread
                     (parameterize ([current-input-port to-use]
                                    [current-output-port from-use]
                                    [current-error-port err-use])
                       (thread (λ ()
                                 (with-handlers
                                   ([(λ (exn) #t)
                                     (λ (exn)
                                       (set-box! err-box exn)
                                       (eprintf "~a~n" (exn->string exn)))])
                                   (let* ([argl (pipeline-member-spec-argl m)]
                                          [thread-ret (apply (car argl) (cdr argl))])
                                     (set-box! ret-box thread-ret)))
                                 (close-input-port (current-input-port))
                                 (close-output-port (current-output-port))
                                 (close-output-port (current-error-port)))))]
                    [ret-member (pmi to-ret from-ret err-ret ret-thread ret-box err-box)])
               (cons ret-member m-outs))))]
        [else (cons m m-outs)])))
  (define r2-members (reverse r2-members-rev))
  (define from-port (pmi-port-from (car r2-members-rev)))
  (define to-port (pmi-port-from (car r2-members)))
  (define (finalize-member m)
    (pipeline-member
     (pmi-subproc-or-thread m)
     (pmi-port-err m)
     (pmi-thread-ret-box m)
     (pmi-thread-exn-box m)))
  (define out-members (map finalize-member r2-members))
  (list out-members to-port from-port))


;;;; Misc funcs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (shellify f)
  (λ args
    (let* ([in-str (port->string (current-input-port))]
           [out-str (apply f in-str args)])
      (display out-str)
      (flush-output)
      0)))

(define (->string a)
  (cond [(string? a) a]
        [(symbol? a) (symbol->string a)]
        [(number? a) (number->string a)]
        [else (format "~a" a)]))

(define (subprocess+ #:in [in #f] #:out [out #f] #:err [err #f] argv)
  (when (null? argv)
    (error 'subprocess+ "empty argv"))
  (define cmd (car argv))
  (define args (cdr argv))
  (define cmdpath (resolve-command-path cmd))
  (when (not cmdpath) (error 'subprocess+ "Command `~a` not in path." cmd))
  (apply subprocess
         (append (list out
                       in
                       err
                       cmdpath)
                 (map ->string args))))


