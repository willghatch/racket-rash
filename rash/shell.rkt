#lang racket/base

(require racket/port)
(require racket/system)
(require ffi/unsafe)
(require racket/list)
(require racket/os)
(require racket/file)
(require racket/function)
(require racket/string)

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

(provide
 make-run-pipeline
 make-run-pipeline->output
 shellify
 )

(define real-output-port (current-output-port))


(define (subprocess+ #:in [in #f] #:out [out #f] #:err [err #f] argv)
  (when (null? argv)
    (error 'subprocess+ "empty argv"))
  (define cmd (car argv))
  (define args (cdr argv))
  (define (convert-arg a)
    (cond [(string? a) a]
          [(symbol? a) (symbol->string a)]
          [(number? a) (number->string a)]
          [else (format "~a" a)]))
  (define cmdpath (find-executable-path (convert-arg cmd)))
  (when (not cmdpath) (error 'subprocess+ "Command `~a` not in path." cmd))
  (apply subprocess
         (append (list out
                       in
                       err
                       (find-executable-path (convert-arg cmd)))
                 (map convert-arg args))))




(struct pipeline-member
  (port-to port-from port-err
           subprocess thread thread-ret-box thread-exn-box
           spec?)
  #:transparent)
(define (pipeline-member-process? pmember)
  (and (pipeline-member? pmember) (pipeline-member-subprocess pmember)))
(define (pipeline-member-thread? pmember)
  (and (pipeline-member? pmember) (pipeline-member-thread pmember)))

(struct pipeline
  (port-to port-from port-err-list members spec? kill-when-end-exits?)
  #:transparent)


(define (pipeline-wait pline)
  (for ([m (pipeline-members pline)])
    (pipeline-member-wait m)))
(define (pipeline-end-wait pline)
  (pipeline-member-wait (car (reverse (pipeline-members pline)))))
(define (pipeline-member-wait member)
  (if (pipeline-member-process? member)
      (subprocess-wait (pipeline-member-subprocess member))
      (thread-wait (pipeline-member-thread member))))

(define (pipeline-kill pline)
  (for ([m (pipeline-members pline)])
    (pipeline-member-kill m)))
(define (pipeline-member-kill m)
  (if (pipeline-member-process? m)
      (subprocess-kill (pipeline-member-subprocess m) #t)
      (kill-thread (pipeline-member-thread m))))

(define (pipeline-status pline)
  (pipeline-member-status (car (reverse (pipeline-members pline)))))
(define (pipeline-member-status m)
  (if (pipeline-member-process? m)
      (subprocess-status (pipeline-member-subprocess m))
      (let* ([dead (thread-dead? (pipeline-member-thread m))]
             [ret (unbox (pipeline-member-thread-ret-box m))]
             [err (unbox (pipeline-member-thread-exn-box m))])
        (if (not dead)
            'running
            (or ret err)))))

(define (run-pipeline pipeline-spec)
  (let* ([members (pipeline-members pipeline-spec)]
         [kill-at-end? (pipeline-kill-when-end-exits? pipeline-spec)]
         [to-port (pipeline-port-to pipeline-spec)]
         [to-use (if (and (pipeline-member-process? (car members))
                          (port? to-port)
                          (not (file-stream-port? to-port)))
                     #f
                     to-port)]
         [from-port (pipeline-port-from pipeline-spec)]
         [from-use (if (and (pipeline-member-process? (car (reverse members)))
                            (port? from-port)
                            (not (file-stream-port? from-port)))
                       #f
                       from-port)]
         [run-members (run-pipeline-members members to-use from-use)]
         [err-outs (map pipeline-member-port-err run-members)]
         [to-out (pipeline-member-port-to (car run-members))]
         [from-out (pipeline-member-port-from (car (reverse run-members)))]
         [sanitized (map (λ (m) (struct-copy pipeline-member m
                                             [port-to #f]
                                             [port-from #f]))
                         run-members)]
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
         [pline (pipeline to-ret from-ret err-outs sanitized #f kill-at-end?)]
         [killer (if kill-at-end?
                     (thread (λ () (pipeline-end-wait pline)
                                (pipeline-kill pline)))
                     #f)]
         [pline-with-killer (struct-copy pipeline pline
                                         [kill-when-end-exits? killer])])
    pline-with-killer))

(define (run-pipeline-members pipeline-member-specs to-line-port from-line-port)
  ;; This should only be called by run-pipeline
  ;; Start a the pipeline, return started members.
  ;; To-line-port and from-line-port should be file-stream-ports if they connect
  ;; to processes.
  (define pipeline-length (length pipeline-member-specs))
  (define r1-members-rev
    (for/fold ([m-outs '()])
              ([m pipeline-member-specs]
               [i (in-range pipeline-length)])
      (cond
        ;; leave thread starting to round 2
        [(pipeline-member-thread? m) (cons m m-outs)]
        [else
         (let* ([to-spec (cond [(null? m-outs)
                                to-line-port]
                               [(pipeline-member-process? (car m-outs))
                                (pipeline-member-port-from (car m-outs))]
                               [else #f])]
                [from-spec (if (equal? i (sub1 pipeline-length))
                               from-line-port
                               #f)]
                [err-spec (pipeline-member-port-err m)]
                [err-to-send (if (and (port? err-spec)
                                      (not (file-stream-port? err-spec)))
                                 #f
                                 err-spec)])
           (let-values ([(sproc from to err-from)
                         (subprocess+ (pipeline-member-subprocess m)
                                      #:err err-to-send
                                      #:in to-spec
                                      #:out from-spec)])
             (let ([out-member (pipeline-member to from err-from sproc #f #f #f #f)])
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
        [(pipeline-member-process? m) (cons m m-outs)]
        [else
         (let* ([prev (and (not (null? m-outs))
                           (car m-outs))]
                [next (and (< i (sub1 pipeline-length))
                           (list-ref r1-members i))]
                [next-is-process? (and next (pipeline-member-process? next))]
                [err-spec (pipeline-member-port-err m)]
                [ret-box (box #f)]
                [err-box (box #f)])
           (let-values ([(to-use to-ret)
                         (cond
                           [(and (not prev) (not to-line-port)) (make-pipe)]
                           [prev (values (pipeline-member-port-from prev) #f)]
                           [else (values to-line-port #f)])]
                        [(from-ret from-use)
                         (cond
                           [next-is-process? (values #f (pipeline-member-port-to next))]
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
                                     (λ (exn) (set-box! err-box exn))])
                                   (let ([thread-ret {(pipeline-member-thread m)}])
                                     (set-box! ret-box thread-ret))))))]
                    [ret-member (pipeline-member to-ret from-ret err-ret
                                                 #f ret-thread ret-box err-box
                                                 #f)])
               (cons ret-member m-outs))))])))
  (define r2-members (reverse r2-members-rev))
  r2-members)

(define (make-pipeline-member-spec s-exp-or-thunk
                                   #:err [err #f])
  (if (procedure? s-exp-or-thunk)
      (pipeline-member #f #f err #f s-exp-or-thunk #f #f #t)
      (pipeline-member #f #f err s-exp-or-thunk #f #f #f #t)))
(define (make-pipeline-spec #:in [in #f]
                            #:out [out #f]
                            #:kill-when-end-exits? [kill-at-end? #t]
                            . members)
  (pipeline in out #f (map make-pipeline-member-spec members) #t kill-at-end?))

(define (make-run-pipeline . specs)
  (let* ([pspec (apply make-pipeline-spec specs
                       #:in (current-input-port)
                       #:out (current-output-port)
                       #:kill-when-end-exits? #t)]
         [pspec (struct-copy pipeline pspec
                             [members (map (λ (m)
                                             (struct-copy
                                              pipeline-member m
                                              [port-err (current-error-port)]))
                                           (pipeline-members pspec))])]
         [pline (run-pipeline pspec)])
    (pipeline-end-wait pline)))

(define (make-run-pipeline->output #:in [in ""]. specs)
  (let* ([out (open-output-string)]
         [err (open-output-string)]
         [in-use (if (string? in)
                     (open-input-string in)
                     in)]
         [pspec (apply make-pipeline-spec specs
                       #:in in-use
                       #:out out)]
         [pspec (struct-copy pipeline pspec
                             [members (map (λ (m)
                                             (struct-copy
                                              pipeline-member m
                                              [port-err err]))
                                           (pipeline-members pspec))])]
         [pline (run-pipeline pspec)]
         ;; TODO - if a pipeline ends with `head -n 1` and a middle process is taking
         ;; forever spitting out tons of stuff, it keeps going.  In bash the pipeline
         ;; would end quickly.
         [wait (pipeline-end-wait pline)]
         [status (pipeline-status pline)]
         )
    (if (not (equal? 0 status))
        (error 'make-run-pipeline->output
               "nonzero pipeline exit (~a) with stderr: ~v"
               status
               (get-output-string err))
        (get-output-string out))))

(define (shellify f)
  (λ ()
    (let* ([in-str (port->string (current-input-port))]
           [out-str (f in-str)])
      (display out-str)
      (flush-output)
      0)))


(module+ main

  (define (grep-func regex str)
    (string-append
     (string-join (filter identity
                          (for/list ([line (string-split str "\n")])
                            (and (regexp-match regex line) line)))
                  "\n")
     "\n"))
  (define (my-grep regex)
    (shellify (λ (str) (grep-func regex str))))


  ;(make-run-pipeline '(ls -l /dev) '(grep tty))
  ;(make-run-pipeline '(ls -l /dev) (shellify (curry grep-func "tty")))
  ;(make-run-pipeline '(ls -l /dev) (my-grep "uucp"))
  ;(with-output-to-string (λ () (make-run-pipeline '(ls -l /dev) (my-grep "uucp"))))
  ;(make-run-pipeline->output '(ls -l /dev) '(grep uucp))
  ;(make-run-pipeline->output '(ls -l /dev) '(grep "this string will not be there"))
  (make-run-pipeline '(find /) '(head -n 1))

  (define (vim-some-rc-file)
    (define file (string-trim
                  (make-run-pipeline->output
                   `(find ,(expand-user-path "~") -maxdepth 1 -regex ".*rc")
                   '(sort)
                   '(head -n 1))))
    (make-run-pipeline `(vim ,file)))
  ;(vim-some-rc-file)

  ;; open a random file from etc
  #;(make-run-pipeline `(vim ,(string-trim
                             (make-run-pipeline->output
                              '(find /etc -maxdepth 1 -type f)
                              '(shuf)
                              '(head -n 1)))))

  (make-run-pipeline->output '(ls -l /dev) (my-grep "uucp"))

  #|
  TODO
  - at some point job control should be possible, in which case it should be
    possible to suspend/resume jobs, send them to the background/foreground,
    stop them, and disown them.  Disowning will probably only work for pipelines
    without any racket functions or filters in them.  But perhaps you should
    be able to mark a pipeline as disownable, which would start a new racket
    process which would run the pipeline.  That might be difficult -- perhaps
    some closure is used in the pipeline -- how could it be copied to the new
    racket process?  It looks like bash and zsh can disown a backgrounded shell
    function and have it survive the shell exiting if that function was started
    in the background, which tells me that backgrounded bash/zsh functions are
    run in a subshell.  Presumably this means the process is forked, so that
    the subshell can still access all previously defined functions.  I feel
    like this could have many cases of subtle weirdness, so I'm not sure I
    want to follow that direction without something more explicit marking a
    clear boundary.  Also, fork() only works on Unix, and it would be nice if
    the shell worked on Windows too, so if I can reasonably avoid relying on
    fork, I should.

  - I ought to wrap subprocess calls and function calls into some
    wrapper struct that I can have the pipeline function inspect to determine
    how to launch each one.  These should probably be called "jobs".  Should
    everything run from the top level of a script/interaction should be a job?
    There could be pipelines and whatnot started below the top level, maybe in
    a top-level function call wrapped in @().  But unless it is wrappen in a form
    that marks it as a job, I don't think it should be.  The function will
    return even if it has eg. a thread or something in it.  But maybe if a top-level
    function returns a thread it should be counted as a job?  But at the same time,
    it may start a thread, but not return it.  I don't think there is really
    anything to be done about this -- a user could, if desired, wrap calls that
    might start weird threads or something in a sandbox.

  |#


  )


