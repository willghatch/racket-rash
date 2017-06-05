#lang racket/base

(require racket/port)
(require racket/exn)
(require racket/list)
(require racket/format)
(require racket/contract)
(require syntax/parse/define)

(provide
 (contract-out

  [shellify (-> procedure?
                procedure?)]

  [struct alias-func ([func procedure?])]
  [struct pipeline-same-thread-func ([func procedure?])]

  [shell-cd (->* () ((or/c string? path? symbol?)) void?)]

  [run-pipeline (->* ()
                     (#:in (or/c input-port? false/c path-string-symbol?)
                      #:out (or/c output-port? false/c path-string-symbol?
                                  (list/c path-string-symbol?
                                          (or/c 'error 'append 'truncate)))
                      #:end-exit-flag any/c
                      #:status-and? any/c
                      #:background? any/c
                      #:default-err (or/c output-port? false/c path-string-symbol?
                                          (list/c path-string-symbol?
                                                  (or/c 'error 'append 'truncate)))
                      )
                     #:rest (listof (or/c list? pipeline-member-spec?))
                     pipeline?)]
  [run-pipeline/out (->* ()
                         (#:in (or/c input-port? false/c path-string-symbol?)
                          #:end-exit-flag any/c
                          #:status-and? any/c)
                         #:rest (listof (or/c list? pipeline-member-spec?))
                         any/c)]
  [run-pipeline/return (->* ()
                            (#:in (or/c input-port? false/c path-string-symbol?)
                             #:out (or/c output-port? false/c path-string-symbol?
                                         (list/c path-string-symbol?
                                                 (or/c 'error 'append 'truncate)))
                             #:end-exit-flag any/c
                             #:status-and? any/c
                             #:failure-as-exn? any/c
                             #:default-err (or/c output-port? false/c path-string-symbol?
                                                 (list/c path-string-symbol?
                                                         (or/c 'error 'append 'truncate)))
                             )
                            #:rest (listof (or/c list? pipeline-member-spec?))
                            any/c)]
  [struct pipeline-member-spec ([argl (listof any/c)]
                                [port-err
                                 (or/c output-port? false/c
                                       path-string-symbol?
                                       (list/c path-string-symbol?
                                               (or/c 'error 'append 'truncate)))])]

  [pipeline? (-> any/c boolean?)]
  [pipeline-port-to (-> pipeline? (or/c false/c output-port?))]
  [pipeline-port-from (-> pipeline? (or/c false/c input-port?))]
  [pipeline-err-ports (-> pipeline? (listof (or/c false/c input-port?)))]
  [pipeline-wait (-> pipeline? void?)]
  [pipeline-kill (-> pipeline? void?)]
  [pipeline-running? (-> pipeline? boolean?)]
  [pipeline-status (-> pipeline? any/c)]
  [pipeline-status/list (-> pipeline? (listof any/c))]
  [pipeline-success? (-> pipeline? any/c)]
  [pipeline-has-failures? (-> pipeline? any/c)]

  [path-string-symbol? (-> any/c boolean?)]
  )
 pipeline-error-captured-stderr
 pipeline-error-argl

 and/success
 or/success
 )


;;;; Command Resolution ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (resolve-alias pm-spec)
  (if (pm-spec-alias? pm-spec)
      (let* ([old-argl (pipeline-member-spec-argl pm-spec)]
             [old-cmd (car old-argl)]
             [new-argl (apply old-cmd (cdr old-argl))]
             [error? (if (or (not (list? new-argl)) (null? new-argl))
                         (error 'resolve-alias "alias did not produce an argument list")
                         #f)]
             [new-cmd (car new-argl)]
             [self-alias? (if (equal? old-cmd new-cmd)
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
  (let ([pathstr (if (path? cmd) cmd (~a cmd))])
    (or (find-executable-path pathstr)
        (and (equal? 'windows (system-type 'os))
             (string? pathstr)
             (find-executable-path
              (string-append (~a cmd) ".exe")))
        (error 'resolve-command-path "can't find executable for ~s" cmd))))

(define (resolve-spec pm-spec)
  (let* ([argl (pipeline-member-spec-argl pm-spec)]
         [cmd (first argl)])
    (cond
      [(pm-spec-alias? pm-spec) (resolve-spec (resolve-alias pm-spec))]
      [(symbol? cmd)
       (resolve-pipeline-member-spec-path pm-spec)]
      ;; Note that paths are safe from further resolution -- an alias
      ;; chain should always end with a path.
      [else pm-spec])))

;;;; Pipeline Members ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct pipeline-member
  (subproc-or-thread port-err thread-ret-box thread-exn-box argl stderr-capture-port)
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
       (let ([cmd (car (pipeline-member-spec-argl pmember))])
         (or (path? cmd) (string? cmd)))))
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
      (let ([thread (pipeline-member-subproc-or-thread m)])
        (when (not (thread-dead? thread))
          (kill-thread (pipeline-member-subproc-or-thread m))
          (set-box! (pipeline-member-thread-exn-box m) 'killed)))))

(define (pipeline-member-running? m)
  (if (pipeline-member-process? m)
      (equal? 'running (subprocess-status (pipeline-member-subproc-or-thread m)))
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

(define (pipeline-member-captured-stderr m)
  (if (and (output-port? (pipeline-member-stderr-capture-port m))
           (string-port? (pipeline-member-stderr-capture-port m)))
      (get-output-string (pipeline-member-stderr-capture-port m))
      #f))

(define (pipeline-member-success? m)
  (if (pipeline-member-process? m)
      (equal? 0 (subprocess-status (pipeline-member-subproc-or-thread m)))
      (not (unbox (pipeline-member-thread-exn-box m)))))

;;;; Pipelines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct pipeline
  (port-to port-from members end-exit-flag start-bg? status-and? from-port-copier err-port-copiers)
  #:property prop:evt (λ (pline)
                        (let ([sema (make-semaphore)])
                          (thread (λ ()
                                    (pipeline-wait pline)
                                    (semaphore-post sema)))
                          (wrap-evt sema (λ _ pline))))
  #:methods gen:custom-write
  [(define (write-proc pline output-port output-mode)
     (if (pipeline-running? pline)
         (fprintf output-port "#<pipeline:running=#t>")
         (fprintf output-port "#<pipeline:success=~a,return=~a>"
                  (pipeline-success? pline)
                  (pipeline-status pline))))]
  ;#:transparent
  )

(define (pipeline-spec? pline)
  (for/and ([m (pipeline-members pline)])
    (pipeline-member-spec? m)))

(define (pipeline-err-ports pline)
  (map pipeline-member-port-err (pipeline-members pline)))

(define (pipeline-wait/all pline)
  (for ([m (pipeline-members pline)])
    (pipeline-member-wait m))
  (pipeline-wait-port-copy/all pline))
(define (pipeline-wait/end pline)
  (pipeline-member-wait (car (reverse (pipeline-members pline))))
  (pipeline-wait-port-copy/end pline))
(define (pipeline-wait-port-copy/end pline)
  (define ts (filter thread? (list (pipeline-from-port-copier pline)
                                   (first (reverse (pipeline-err-port-copiers pline))))))
  (for ([t ts])
    (thread-wait t)))
(define (pipeline-wait-port-copy/all pline)
  (define ts (filter thread? (cons (pipeline-from-port-copier pline)
                                   (pipeline-err-port-copiers pline))))
  (for ([t ts])
    (thread-wait t)))

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
  (pipeline-wait/all pline)
  (map pipeline-member-status (pipeline-members pline)))
(define (pipeline-status/and pline)
  (pipeline-wait/all pline)
  (let ([first-error (ormap (λ (m) (and (not (pipeline-member-success? m))
                                        (pipeline-member-status m)))
                            (pipeline-members pline))])
    (or first-error
        (pipeline-member-status (car (reverse (pipeline-members pline)))))))
(define (pipeline-status/end pline)
  (pipeline-wait/end pline)
  (pipeline-member-status (car (reverse (pipeline-members pline)))))
(define (pipeline-status pline)
  (if (pipeline-status-and? pline)
      (pipeline-status/and pline)
      (pipeline-status/end pline)))

(define (pipeline-error-captured-stderr/and pline)
  (pipeline-wait/all pline)
  (ormap (λ (m) (and (not (pipeline-member-success? m))
                     (pipeline-member-captured-stderr m)))
         (pipeline-members pline)))
(define (pipeline-error-captured-stderr/end pline)
  (pipeline-wait pline)
  (pipeline-member-captured-stderr (car (reverse (pipeline-members pline)))))
(define (pipeline-error-captured-stderr pline)
  (if (pipeline-status-and? pline)
      (pipeline-error-captured-stderr/and pline)
      (pipeline-error-captured-stderr/end pline)))

(define (pipeline-error-argl/and pline)
  (pipeline-wait/all pline)
  (ormap (λ (m) (and (not (pipeline-member-success? m))
                     (pipeline-member-argl m)))
         (pipeline-members pline)))
(define (pipeline-error-argl/end pline)
  (pipeline-wait pline)
  (pipeline-member-argl (car (reverse (pipeline-members pline)))))
(define (pipeline-error-argl pline)
  (if (pipeline-status-and? pline)
      (pipeline-error-argl/and pline)
      (pipeline-error-argl/end pline)))

(define (pipeline-has-failures? pline)
  (ormap (λ (m) (not (pipeline-member-success? m)))
         (pipeline-members pline)))

(define (pipeline-success? pline)
  (pipeline-wait pline)
  (if (pipeline-status-and? pline)
      (for/and ([m (pipeline-members pline)])
        (pipeline-member-success? m))
      (pipeline-member-success? (car (reverse (pipeline-members pline))))))


(define (make-pipeline-spec #:in [in (current-input-port)]
                            #:out [out (current-output-port)]
                            #:end-exit-flag [end-exit-flag #t]
                            #:status-and? [status-and? #f]
                            #:background? [bg? #f]
                            #:default-err [default-err (current-error-port)]
                            members)
  (let* ([members1 (map (λ (m)
                          (if (pipeline-member-spec? m)
                              m
                              (pipeline-member-spec m default-err)))
                        members)]
         [members2 (map (λ (m)
                          (if (equal? 'default (pipeline-member-spec-port-err m))
                              (struct-copy pipeline-member-spec m
                                           [port-err default-err])
                              m))
                        members1)])
    (pipeline in out
              members2
              end-exit-flag bg? status-and?
              #f #f)))

(define (run-pipeline #:in [in (current-input-port)]
                      #:out [out (current-output-port)]
                      #:end-exit-flag [end-exit-flag #t]
                      #:status-and? [status-and? #f]
                      #:background? [bg? #f]
                      ;; TODO -- allow 'string-port
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
               pline))))

(define (run-pipeline/out #:end-exit-flag [end-exit-flag #t]
                          #:status-and? [status-and? #f]
                          #:in [in (open-input-string "")]
                          . members)
  (let* ([out (open-output-string)]
         [err (open-output-string)]
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
    (if (not (pipeline-success? pline))
        (error 'run-pipeline/out
               "unsuccessful pipeline with return ~a and stderr: ~v"
               status
               (get-output-string err))
        (get-output-string out))))

(define (run-pipeline/return #:in [in (current-input-port)]
                             #:out [out (current-output-port)]
                             #:end-exit-flag [end-exit-flag #t]
                             #:status-and? [status-and? #f]
                             #:default-err [default-err (current-error-port)]
                             #:failure-as-exn? [failure-as-exn? #t]
                             . members)
  (let ([pline (apply run-pipeline
                      #:in in #:out out
                      #:default-err default-err
                      #:end-exit-flag end-exit-flag
                      #:status-and? status-and?
                      #:background? #f
                      members)])
    (pipeline-wait pline)
    (if (or (pipeline-success? pline) (not failure-as-exn?))
        (pipeline-status pline)
        (let ([err (pipeline-status pline)])
          (if (exn? err)
              (raise err)
              (error 'run-pipeline/return
                     "pipeline unsuccessful with return ~a" err))))))


(define (run-pipeline/spec pipeline-spec)
  (let* ([members-pre-resolve (pipeline-members pipeline-spec)]
         [members (map resolve-spec members-pre-resolve)]
         [kill-flag (pipeline-end-exit-flag pipeline-spec)]
         [status-and? (pipeline-status-and? pipeline-spec)]
         [bg? (pipeline-start-bg? pipeline-spec)]
         [to-port (pipeline-port-to pipeline-spec)]
         [to-file-port (cond [(equal? to-port 'null) (open-input-string "")]
                             [(path-string-symbol? to-port)
                              (open-input-file (path-string-sym->path to-port))]
                             [else #f])]
         ;; re-use name...
         [to-port (or to-file-port to-port)]
         [to-use (cond [(and (pm-spec-path? (car members))
                             (port? to-port)
                             (not (file-stream-port? to-port)))
                        #f]
                       [else to-port])]
         [from-port (pipeline-port-from pipeline-spec)]
         [from-file-port (with-handlers ([(λ _ #t) (λ (e)
                                                     (when (port? to-file-port)
                                                       (close-input-port to-file-port))
                                                     (raise e))])
                           (cond [(equal? from-port 'null) (open-output-nowhere)]
                                 [(path-string-symbol? from-port)
                                  (open-output-file
                                   (path-string-sym->path from-port)
                                   #:exists 'error)]
                                 [(list? from-port)
                                  (open-output-file
                                   (path-string-sym->path (first from-port))
                                   #:exists (second from-port))]
                                 [else #f]))]
         ;; re-use from-port name
         [from-port (or from-file-port from-port)]
         [from-use (cond [(and (pm-spec-path? (car (reverse members)))
                               (port? from-port)
                               (not (file-stream-port? from-port)))
                          #f]
                         [else from-port])]
         [err-ports (map pipeline-member-spec-port-err members)]
         [err-ports-with-paths (map (λ (p) (cond [(path-string-symbol? p)
                                                  (path-string-sym->path p)]
                                                 [(list? p)
                                                  (path-string-sym->path (car p))]
                                                 [else #f]))
                                    err-ports)]
         [err-ports-mapped-with-dup-numbers
          (for/list ([p err-ports]
                     [epp err-ports-with-paths]
                     [i (in-naturals)])
            (with-handlers ([(λ _ #t)(λ (e) e)])
              ;; if an exception is thrown, catch it and save it for later
              (cond
                [(equal? p 'stdout) p]
                [(equal? p 'string-port) (open-output-string)]
                [(equal? p 'null) (open-output-nowhere)]
                ;; If this is the second instance of the file, note the number
                ;; so they can share ports rather than trying to open
                ;; a file twice.
                [(and (path? epp) (member epp (take err-ports-with-paths i)))
                 (- (length err-ports-with-paths)
                    (length (member epp err-ports-with-paths)))]
                [(list? p) (open-output-file epp #:exists (second p))]
                [(path-string-symbol? p)
                 (open-output-file (path-string-sym->path p)
                                   #:exists 'error)]
                [else p])))]
         [err-ports-mapped (map (λ (p) (if (number? p)
                                           (list-ref err-ports-mapped-with-dup-numbers p)
                                           p))
                                err-ports-mapped-with-dup-numbers)]
         [err-ports-to-close (filter (λ (x) x)
                                     (map (λ (p pstr?) (if (and (port? p) pstr?) p #f))
                                          err-ports-mapped err-ports-with-paths))]
         [err-ports-err-close-for-exn
          ;; ok, now that we have a reference to any opened error ports, we can
          ;; close them before throwing out our error.
          (map (λ (p) (if (exn? p)
                          (begin (for ([p err-ports-to-close])
                                   (close-output-port p))
                                 (when from-file-port (close-output-port from-file-port))
                                 (when to-file-port (close-input-port to-file-port))
                                 (raise p))
                          #f))
               err-ports-mapped)]
         [members-with-m-err
          (map (λ (m p) (struct-copy pipeline-member-spec m
                                     [port-err p]))
               members
               err-ports-mapped)]
         [run-members/ports (run-pipeline-members members-with-m-err to-use from-use)]
         [run-members (first run-members/ports)]
         [to-out (second run-members/ports)]
         [from-out (third run-members/ports)]
         [err-port-copiers (fourth run-members/ports)]
         [ports-to-close (append (filter port? (list to-file-port from-file-port))
                                 err-ports-to-close)]
         [from-ret-almost (if (and from-port from-out)
                              (thread (λ ()
                                        (copy-port from-out from-port)
                                        (close-input-port from-out)))
                              from-out)]
         [from-port-copier (if (thread? from-ret-almost)
                              from-ret-almost
                              #f)]
         [from-ret (if (thread? from-ret-almost) #f from-ret-almost)]
         [to-ret (if (and to-port to-out)
                     (begin
                       (thread (λ ()
                                 (copy-port to-port to-out)
                                 (close-output-port to-out)))
                       #f)
                     to-out)]
         [pline (pipeline to-ret from-ret run-members kill-flag bg? status-and?
                          from-port-copier err-port-copiers)]
         [killer (if (or (equal? kill-flag 'always)
                         (and kill-flag
                              (not status-and?)))
                     (thread (λ () (pipeline-wait pline)
                                (pipeline-kill pline)))
                     #f)]
         [closer (if (not (empty? ports-to-close))
                     (thread (λ ()
                               (pipeline-wait pline)
                               (for ([p ports-to-close])
                                 (when (output-port? p) (close-output-port p))
                                 (when (input-port? p) (close-input-port p)))))
                     #f)]
         [pline-final (struct-copy pipeline pline
                                   [end-exit-flag killer])])
    pline-final))

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
  |#
  (struct pmi
    ;; pmi for pipeline-member-intermediate -- has info important for running
    ;; things that I don't want to expose in the final output.
    (port-to port-from port-err subproc-or-thread thread-ret-box thread-exn-box argl capture-port-err)
    #:transparent)
  (define (pmi-process? pmember)
    (and (pmi? pmember) (subprocess? (pmi-subproc-or-thread pmember))))
  (define (pmi-thread? pmember)
    (and (pmi? pmember) (thread? (pmi-subproc-or-thread pmember))))

  (define pipeline-length (length pipeline-member-specs))
  (define-values (r1-members-rev err-port-copy-threads-rev)
    (for/fold ([m-outs '()]
               [err-port-copy-threads '()])
              ([m pipeline-member-specs]
               [i (in-range pipeline-length)])
      (cond
        ;; leave thread starting to round 2
        [(pm-spec-func? m) (values (cons m m-outs) (cons #f err-port-copy-threads))]
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
                                 err-spec)]
                [capture-err (if (and (output-port? err-spec)
                                      (string-port? err-spec))
                                 err-spec
                                 #f)]
                [argl (pipeline-member-spec-argl m)])
           (let-values ([(sproc from to err-from)
                         (subprocess+ argl
                                      #:err err-to-send
                                      #:in to-spec
                                      #:out from-spec)])
             (when (and to-spec (> i 0)) (close-input-port to-spec))
             (let ([out-member (pmi to from err-from sproc #f #f argl capture-err)])
               (values
                (cons out-member m-outs)
                (cons (if (and err-spec err-from)
                          (thread (λ ()
                                    (copy-port err-from err-spec)
                                    (close-input-port err-from)))
                          #f)
                      err-port-copy-threads)))))])))

  (define r1-members (reverse r1-members-rev))
  (define err-port-copy-threads (reverse err-port-copy-threads-rev))

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
                [err-box (box #f)]
                [capture-err (and (output-port? err-spec)
                                  (string-port? err-spec)
                                  err-spec)]
                [argl (pipeline-member-spec-argl m)])
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
                                     ret-thread ret-box err-box argl capture-err)])
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
     (pmi-thread-exn-box m)
     (pmi-argl m)
     (pmi-capture-port-err m)))

  (define out-members (map finalize-member r3-members))
  (list out-members to-port from-port err-port-copy-threads))


;;;; Misc funcs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (shellify f)
  (λ args
    (let* ([in-str (port->string (current-input-port))]
           [out-str (apply f in-str args)])
      (display out-str)
      (flush-output))))

(define (path-string-symbol? pss)
  (or (path-string? pss)
      (and (symbol? pss) (path-string? (symbol->string pss)))))
(define (path-string-sym->path pss)
  (cond [(symbol? pss) (string->path (symbol->string pss))]
        [(string? pss) (string->path pss)]
        [else pss]))

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
                 (map ~a args))))

;;;; and/or macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-simple-macro (and/success e ...)
  (and (let ([tmp e]) (if (pipeline? tmp) (and (pipeline-success? tmp) tmp) tmp)) ...))
(define-simple-macro (or/success e ...)
  (or (let ([tmp e]) (if (pipeline? tmp) (and (pipeline-success? tmp) tmp) tmp)) ...))

;;;; Base Shell Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (change-current-directory dir)
  (if (directory-exists? dir)
      (current-directory dir)
      (error 'change-current-directory "No such directory: ~a" dir)))

(define shell-cd
  (pipeline-same-thread-func
   (λ ([dir ""])
     (let ([dir (if (equal? dir "") (getenv "HOME") dir)])
       (change-current-directory
        (cond [(string? dir) dir]
              [(path? dir) dir]
              [(symbol? dir) (symbol->string dir)]
              [else (error 'cd "cd argument needs to be a string, path, or symbol")]))))))

