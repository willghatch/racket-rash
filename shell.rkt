#lang racket/base

(require racket/port)
(require racket/system)
(require ffi/unsafe)
(require racket/list)
(require basedir)
(require racket/os)
(require racket/file)

(provide subprocess+
         pcom/standard
         )

(define real-output-port (current-output-port))

(define (subprocess/path out in err cmd . args)
  (apply subprocess (append (list out in err (find-executable-path cmd)) args)))

(define (port->file-stream-port p)
  (cond [(and (port? p) (file-stream-port? p)) p]
        [(input-port? p)
         (let-values ([(from-pipe to-pipe) (open-os-fifo)])
           (thread (λ ()
                     (copy-port p to-pipe)
                     (close-output-port to-pipe)))
           from-pipe)]
        [(output-port? p)
         (let-values ([(from-pipe to-pipe) (open-os-fifo)])
           (thread (λ ()
                     (copy-port from-pipe p)
                     (close-input-port from-pipe)))
           to-pipe)]
        ;[(string? p) aoeu]
        [(not p) #f]
        [else (error 'port->file-stream-port "not a port: ~a" p)]))

(define fifo-executor-thread #f)
(define fifo-executor #f)
(define mkfifo
  (get-ffi-obj "mkfifo" #f
               (_fun _string _int
                     -> [ret : _int]
                     -> (if (equal? 0 ret)
                            (void)
                            (error 'mkfifo "unable to create fifo")))))
(define (open-os-fifo)
  (define fifo-name
    (writable-runtime-file
     #:program "racket-shell"
     (string-append "pid" (number->string (getpid)) "/"
                    "fifo_ms" (number->string (current-milliseconds))
                    "_r" (number->string (random 1 4123456789)))))
  (make-parent-directory* fifo-name)
  (mkfifo fifo-name #o700)
  (let* ([to-fifo (open-output-file fifo-name #:exists 'append)]
         [from-fifo (open-input-file fifo-name)])
    ;; TODO - these should be cleaned up at some point
    (when (not fifo-executor-thread)
      (set! fifo-executor (make-will-executor))
      (set! fifo-executor-thread
            (thread (λ () (let loop ()
                            (will-execute fifo-executor)
                            (loop))))))
    ;; use wills to eventually remove the fifo
    ;; TODO -- this isn't working.  I really want to register with the
    ;; custodian that the file should be removed after the ports are closed.
    ;; Is that possible?
    (define flag (box #t))
    (define from-flag (box flag))
    (define to-flag (box flag))
    (will-register fifo-executor from-fifo (λ (_) (println "cleaning from fifo" real-output-port)(set-box! from-flag #f)))
    (will-register fifo-executor to-fifo (λ (_) (set-box! to-flag #f)))
    (will-register fifo-executor flag (λ (_) (delete-file fifo-name)))
    (values from-fifo to-fifo)))


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
         (append (list (port->file-stream-port out)
                       (port->file-stream-port in)
                       (port->file-stream-port err)
                       (find-executable-path (convert-arg cmd)))
                 (map convert-arg args))))

(define (pcom/standard program . args)
  (let-values ([(sp i o e)
                (subprocess+ (cons program args)
                             #:in (current-input-port)
                             #:out (current-output-port)
                             #:err (current-error-port)
                             )])
    (subprocess-wait sp)
    (subprocess-status sp)))


(define (pipe2 argv1 argv2 #:in [in #f] #:out [out #f] #:err1 [err1 #f] #:err2 [err2 #f])
  (let-values ([(sproc1 from1 to1 err1-out)
                (subprocess+ argv1 #:in in #:err err1)])
    (let-values ([(sproc2 from2 to2 err2-out)
                  (subprocess+ argv2 #:in from1 #:out out #:err err2)])
      ;; TODO - I really need an event that represents both processes for
      ;; done-ness but can query the return value of the second one.
      (values sproc2 to1 from2 err1-out err2-out)
      )))

(define (pipeline #:in [in #f] #:out [out #f] #:errs [errs #f] argvs)
  (let* ([n-argv (length argvs)]
         [zero-argvs? (if (equal? 0 n-argv)
                          (error 'pipeline "Needs at least one subprocess specification.")
                          #f)]
         [errspec-list (if errs errs (make-list n-argv #f))]
         [list-mismatch? (if (equal? n-argv (length errspec-list))
                             #f
                             (error 'pipeline "Error port specification list didn't match the length of the subprocess list"))])
    (let-values ([(reversed-sprocs reversed-errs reversed-froms)
                  (for/fold ([sprocs-r '()]
                             [errs-r '()]
                             [froms-r (list in)])
                            ([argv-n argvs]
                             [errspec-n errspec-list]
                             [i (in-range n-argv)])
                    (let-values ([(sproc-n from-n to-n err-n)
                                  (subprocess+ argv-n
                                               #:in (first froms-r)
                                               #:out (if (equal? i (sub1 n-argv))
                                                         out
                                                         #f)
                                               #:err errspec-n)])
                      (values (cons sproc-n sprocs-r)
                              (cons err-n errs-r)
                              (cons from-n froms-r))))])
      (values (reverse reversed-sprocs)
              (second (reverse reversed-froms))
              (first reversed-froms)
              (reverse reversed-errs)))))


(define (subprocesses-wait process-list)
  (for ([sp process-list])
    (subprocess-wait sp)))
(define (subprocesses-status/last process-list)
  (subprocess-status (car (reverse process-list))))
(define (subprocesses-status/and process-list)
  (let ([stati (map subprocess-status process-list)])
    (if (member 'running stati)
        'running
        (let ([first-nonzero (memf (λ (x) (not (equal? x 0))) stati)])
          (if first-nonzero first-nonzero 0)))))


(module+ main
  ;(display (pipe2 '(ls /dev) '(grep tty)))

  #;(define-values (proc out in err)
    (subprocess+ #:in (current-input-port) #:out (current-output-port) #:err (current-error-port) '(vim ~/test)))


  #;(subprocess-wait proc)
  ;(pcom/standard 'ls '-l)
  #;(with-output-to-string
    (λ ()
      (define-values (sp o i e) (subprocess+ #:out (current-output-port) '(ls -a)))
      (subprocess-wait sp)
      ))

  ;(collect-garbage)
  ;(pcom/standard 'date)
  ;(sleep 5)
  ;(collect-garbage)
  ;(pcom/standard 'vi)

  (with-output-to-string
    (λ ()
      (define-values (procs to-line from-line errs)
        (pipeline (list '(ls -l) '(grep Jul) '(wc) ) #:out (current-output-port)))
      (subprocesses-wait procs)))


  #|
  TODO
  - I don't need the mkfifo nonsense, I just need to start subprocesses and get
    the file-stream-ports that they give.  I just need to figure out the right
    order to start things in and wire up the ports correctly.  If the whole-pipeline
    input or output go to a non-file-stream port, I can just use copy-port.

  - pipelines should be able to include racket functions, which should probably
    be run in their own threads.  There should be a form to use racket functions
    like processes (eg. they get a stdin, stdout, stderr, return a status of
    some sort), and a form for them to get their stdin as a string argument and
    probably just return a string for their output.

  - the string version can just entail a wrapper function that reads everything
    from a port, passes it as a string, then displays that output to the output port.
  - pipelines should be runnable in the background

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

  - This means I ought to wrap subprocess calls and function calls into some
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


