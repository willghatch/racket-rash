#lang racket/base

(require racket/port)
(require racket/exn)
(require racket/list)
(require racket/contract)

(provide
 (contract-out

  [shellify (-> procedure?
                procedure?)]

  [current-shell-functions (->* ()
                                ((hash/c string? procedure?))
                                (or/c (hash/c string? procedure?) void?))]
  [base-shell-functions (hash/c string? procedure?)]
  [add-shell-function (-> (or/c string? symbol?) procedure? void?)]
  [shell-alias (-> (or/c string? symbol?) (listof any/c) void?)]
  [struct alias-func ([func procedure?])]
  [struct pipeline-same-thread-func ([func procedure?])]

  [shell-cd (->* () #:rest (listof (or/c string? path? symbol?)) integer?)]
  [shell-printf (->* (string?) #:rest (listof any/c) integer?)]
  [shell-echo (->* () #:rest (listof any/c) integer?)]

  [run-pipeline (->* ()
                     (#:in (or/c input-port? false/c)
                      #:out (or/c output-port? false/c)
                      #:end-exit-flag any/c
                      #:status-and? any/c
                      #:background? any/c
                      #:default-err (or/c output-port? false/c 'stdout)
                      )
                     #:rest (listof (or/c list? pipeline-member-spec?))
                     any/c)]
  [run-pipeline/out (->* ()
                         (#:end-exit-flag any/c
                          #:status-and? any/c)
                         #:rest (listof (or/c list? pipeline-member-spec?))
                         any/c)]
  [struct pipeline-member-spec ([argl (listof any/c)]
                                [port-err (or/c output-port? false/c 'stdout)])]

  [pipeline? (-> any/c boolean?)]
  [pipeline-port-to (-> pipeline? (or/c false/c output-port?))]
  [pipeline-port-from (-> pipeline? (or/c false/c input-port?))]
  [pipeline-err-ports (-> pipeline? (listof (or/c false/c input-port?)))]
  [pipeline-wait (-> pipeline? void?)]
  [pipeline-kill (-> pipeline? void?)]
  [pipeline-running? (-> pipeline? boolean?)]
  [pipeline-status (-> pipeline? any/c)]
  [pipeline-status/list (-> pipeline? (listof any/c))]
  ))


;;;; Command Resolution ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-shell-function name function)
  (current-shell-functions (hash-set (current-shell-functions)
                                     (if (symbol? name) (symbol->string name) name)
                                     function)))
(define (shell-alias name argl-start)
  (add-shell-function name (alias-func (λ args (append argl-start args)))))

(define (lookup-shell-function key)
  (let* ([skey (->string key)])
    (hash-ref (current-shell-functions) skey #f)))

(define (resolve-alias pm-spec)
  (if (pm-spec-alias? pm-spec)
      (let* ([old-argl (pipeline-member-spec-argl pm-spec)]
             [old-cmd (car old-argl)]
             [new-argl (apply old-cmd (cdr old-argl))]
             [error? (if (or (not (list? new-argl)) (null? new-argl))
                         (error 'resolve-alias "alias did not produce an argument list")
                         #f)]
             [new-cmd (car new-argl)]
             [self-alias? (if (or (equal? old-cmd new-cmd)
                                  (and (or (string? new-cmd) (symbol? new-cmd))
                                       (equal? (lookup-shell-function new-cmd) old-cmd)))
                              (error 'resolve-alias "alias ~a resolves to itself." new-cmd)
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

(define (resolve-spec pm-spec)
  (let* ([argl (pipeline-member-spec-argl pm-spec)]
         [cmd (first argl)]
         [cmdstr (and (or (symbol? cmd) (string? cmd)) (->string cmd))])
    (cond
      [(pm-spec-alias? pm-spec) (resolve-spec (resolve-alias pm-spec))]
      [cmdstr
       (let ([looked (lookup-shell-function cmdstr)])
         (if looked
             (resolve-spec (struct-copy pipeline-member-spec pm-spec
                                        [argl (cons looked (cdr argl))]))
             (resolve-pipeline-member-spec-path pm-spec)))]
      ;; Note that paths are safe from further resolution -- an alias
      ;; chain should always end with a path.
      [else pm-spec])))

;;;; Pipeline Members ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct pipeline-member
  (subproc-or-thread port-err thread-ret-box thread-exn-box)
  #:transparent)
(struct pipeline-member-spec
  (argl port-err)
  #:transparent)
(struct alias-func
  (func)
  #:property prop:procedure (struct-field-index func)
  #:transparent)
(struct pipeline-same-thread-func
  (func)
  #:property prop:procedure (struct-field-index func)
  #:transparent)

(define (pm-spec-command m)
  (car (pipeline-member-spec-argl m)))
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
(define (pm-spec-same-thread? m)
  (and (pm-spec-func? m)
       (pipeline-same-thread-func? (pm-spec-command m))))

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

(define (pipeline-member-running? m)
  (if (pipeline-member-process? m)
      (equal? 'running (subprocess-status (pipeline-member-subproc-or-thread m) #t))
      (thread-running? (pipeline-member-subproc-or-thread m))))

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
  (port-to port-from members end-exit-flag start-bg? status-and?)
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
  (if (pipeline-status-and? pline)
      (pipeline-wait/all pline)
      (pipeline-wait/end pline)))

(define (pipeline-kill pline)
  (for ([m (pipeline-members pline)])
    (pipeline-member-kill m)))

(define (pipeline-running? pline)
  (for/or ([m (pipeline-members pline)])
    (pipeline-member-running? m)))

#|
TODO - pipeline status should have a special value for pipeline
members that have been killed.  Also, it should count as success for
pipelines where it is set to always kill when the end member exits
|#
(define (pipeline-status/list pline)
  (map pipeline-member-status (pipeline-members pline)))
(define (pipeline-status/and pline)
  (define (rec sl)
    (cond [(null? sl) 0]
          [(equal? 0 (car sl)) (rec (cdr sl))]
          [else (car sl)]))
  (rec (pipeline-status/list pline)))
(define (pipeline-status/end pline)
  (pipeline-member-status (car (reverse (pipeline-members pline)))))
(define (pipeline-status pline)
  (if (pipeline-status-and? pline)
      (pipeline-status/and pline)
      (pipeline-status/end pline)))


(define (make-pipeline-spec #:in [in (current-input-port)]
                            #:out [out (current-output-port)]
                            #:end-exit-flag [end-exit-flag #t]
                            #:status-and? [status-and? #f]
                            #:background? [bg? #f]
                            #:default-err [default-err (current-error-port)]
                            members)
  (pipeline in out
            (map (λ (m)
                   (if (pipeline-member-spec? m)
                       m
                       (pipeline-member-spec m default-err)))
                 members)
            end-exit-flag bg? status-and?))

(define (run-pipeline #:in [in (current-input-port)]
                      #:out [out (current-output-port)]
                      #:end-exit-flag [end-exit-flag #t]
                      #:status-and? [status-and? #f]
                      #:background? [bg? #f]
                      #:default-err [default-err (current-error-port)]
                      . members)
  (let ([pline
         (run-pipeline/spec
          (make-pipeline-spec #:in in #:out out
                              #:end-exit-flag end-exit-flag
                              #:status-and? status-and?
                              #:background? bg?
                              #:default-err default-err
                              members))])
    (if bg?
        pline
        (begin (pipeline-wait pline)
               (pipeline-status pline)))))

(define (run-pipeline/out #:end-exit-flag [end-exit-flag #t]
                          #:status-and? [status-and? #f]
                          . members)
  (let* ([out (open-output-string)]
         [err (open-output-string)]
         [in (open-input-string "")]
         [pline-spec (make-pipeline-spec #:in in #:out out
                                         #:end-exit-flag end-exit-flag
                                         #:status-and? status-and?
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
        (error 'run-pipeline/out
               "nonzero pipeline exit (~a) with stderr: ~v"
               status
               (get-output-string err))
        (get-output-string out))))


(define (run-pipeline/spec pipeline-spec)
  (let* ([members-pre-resolve (pipeline-members pipeline-spec)]
         [members (map resolve-spec members-pre-resolve)]
         [kill-flag (pipeline-end-exit-flag pipeline-spec)]
         [status-and? (pipeline-status-and? pipeline-spec)]
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
         [pline (pipeline to-ret from-ret run-members kill-flag bg? status-and?)]
         [killer (if (or (equal? kill-flag 'always)
                         (and kill-flag
                              (not status-and?)))
                     (thread (λ () (pipeline-wait pline)
                                (pipeline-kill pline)))
                     #f)]
         [pline-with-killer (struct-copy pipeline pline
                                         [end-exit-flag killer])])
    pline-with-killer))

(define (run-pipeline-members pipeline-member-specs to-line-port from-line-port)
  #| This has to be careful about the it starts things in for proper wiring.
  The important bit is that subprocesses have to be passed a file-stream-port
  for each of its in/out/err ports (or #f to create one).
  To facilitate this, we start the subprocess pipeline members first, using
  the previous output as the input if it's a file-stream, and getting an output
  for each (except maybe at the end).  If a process gets its input from a thread
  pipeline member, then the process needs to give us a file-stream port, which we
  then use in the next pass for threads.

  The second pass starts the function/thread pipeline members.

  In the end there is a third pass to start any pipeline members marked to run
  in the same thread, which is basically a hack, but I deemed it worth it to
  let `cd` be run in a pipeline (for syntactic easiness in Rash).

  TODO - I think the intermediate ports between subprocesses still need to
  be closed on the Racket side.  Maybe for simplicity I should just collect
  every file-stream port created while starting ports and make a watcher thread
  that closes them all once the pipeline finishes (except for the external facing
  ones -- if I return a new port for whole-pipeline input/output or an error port,
  those should be closed outside).
  |#
  (struct pmi
    ;; pmi for pipeline-member-intermediate -- has info important for running
    ;; things that I don't want to expose in the final output.
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

  (define (run-func-members members same-thread-ones?)
    (for/fold ([m-outs '()])
              ([m members]
               [i (in-range pipeline-length)])
      (cond
        [(and (pm-spec-func? m)
              (equal? same-thread-ones? (pm-spec-same-thread? m)))
         (let* ([prev (and (not (null? m-outs))
                           (car m-outs))]
                [next (and (< i (sub1 pipeline-length))
                           (list-ref r1-members (add1 i)))]
                [next-is-running? (and next (pmi? next))]
                [prev-is-running? (and prev (pmi? prev))]
                [err-spec (pipeline-member-spec-port-err m)]
                [ret-box (box #f)]
                [err-box (box #f)])
           (let-values ([(to-use to-ret)
                         (cond
                           [prev-is-running? (values (pmi-port-from prev) #f)]
                           [prev (make-pipe)]
                           [(and (not prev) (not to-line-port)) (make-pipe)]
                           [else (values (dup-input-port to-line-port) #f)])]
                        [(from-ret from-use)
                         (cond
                           [next-is-running? (values #f (pmi-port-to next))]
                           [next (make-pipe)]
                           [(not from-line-port) (make-pipe)]
                           [else (values #f (dup-output-port from-line-port))])]
                        [(err-ret err-use)
                         (cond [(output-port? err-spec)
                                (values #f (dup-output-port err-spec))]
                               [(equal? err-spec 'stdout) (values #f err-spec)]
                               [else (make-pipe)])])
             (let* ([ret-thread
                     (parameterize ([current-input-port to-use]
                                    [current-output-port from-use]
                                    [current-error-port (if (equal? err-use 'stdout)
                                                            from-use
                                                            err-use)])
                       (if same-thread-ones?
                           (begin
                             {(mk-run-thunk m err-box ret-box)}
                             (thread (λ () #f)))
                           (thread (mk-run-thunk m err-box ret-box))))]
                    [ret-member (pmi to-ret from-ret err-ret
                                     ret-thread ret-box err-box)])
               (cons ret-member m-outs))))]
        [else (cons m m-outs)])))

  (define (mk-run-thunk m-spec err-box ret-box)
    (λ ()
      (with-handlers
        ([(λ (exn) #t)
          (λ (exn)
            (set-box! err-box exn)
            (eprintf "~a~n" (exn->string exn)))])
        (let* ([argl (pipeline-member-spec-argl m-spec)]
               [thread-ret (apply (car argl) (cdr argl))])
          (set-box! ret-box thread-ret)))
      (close-input-port (current-input-port))
      (close-output-port (current-output-port))
      (close-output-port (current-error-port))))

  (define r2-members-rev (run-func-members r1-members #f))
  (define r2-members (reverse r2-members-rev))
  ;; same-thread function members
  (define r3-members-rev (run-func-members r2-members #t))
  (define r3-members (reverse r3-members-rev))
  (define from-port (pmi-port-from (car r3-members-rev)))
  (define to-port (pmi-port-to (car r3-members)))
  (define (finalize-member m)
    (pipeline-member
     (pmi-subproc-or-thread m)
     (pmi-port-err m)
     (pmi-thread-ret-box m)
     (pmi-thread-exn-box m)))
  (define out-members (map finalize-member r3-members))
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

;;;; Base Shell Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (change-current-directory dir)
  (if (directory-exists? dir)
      (current-directory dir)
      (error 'change-current-directory "No such directory: ~a" dir)))

(define (shell-func-catch-error e)
  (eprintf "~a" (exn->string e))
  1)

(define shell-cd
  (pipeline-same-thread-func
   (λ dirs
     (let ([dir (if (null? dirs) (getenv "HOME") (car dirs))]
           [too-many (if (> (length dirs) 1)
                         (error 'cd "too many arguments")
                         #f)])
       (change-current-directory
        (cond [(string? dir) dir]
              [(path? dir) dir]
              [(symbol? dir) (symbol->string dir)]
              [else (error 'cd "cd argument needs to be a string, path, or symbol")])))
     0)))

(define (shell-printf f-string . args)
  (with-handlers ([(λ _ #t) shell-func-catch-error])
    (apply printf f-string args))
  0)

(define (shell-echo . args)
  (for ([a args]
        [i (in-naturals)])
    (when (not (equal? i 0))
      (display " "))
    (display a))
  (display "\n")
  0)

(define base-shell-functions
  (hash "cd" shell-cd
        "printf" shell-printf
        "echo" shell-echo
        ))

(define current-shell-functions
  (make-parameter base-shell-functions))


