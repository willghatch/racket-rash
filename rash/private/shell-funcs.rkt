#lang racket/base

(require racket/port)
(require racket/exn)
(require racket/function)
(require racket/string)

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))

(provide
 shellify
 rash-pipeline
 rash-pipeline/funcify
 pipeline-wait
 pipeline-kill
 pipeline-status
 pipeline-status/end
 pipeline-status/all
 pipeline-status/list
 )


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
  (define cmdpath (or (find-executable-path (convert-arg cmd))
                      (and (equal? 'windows (system-type 'os))
                           (find-executable-path
                            (string-append (convert-arg cmd) ".exe")))))
  (when (not cmdpath) (error 'subprocess+ "Command `~a` not in path." cmd))
  (apply subprocess
         (append (list out
                       in
                       err
                       cmdpath)
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
  (port-to port-from port-err-list members spec? end-exit-flag start-bg? status-all?)
  #:transparent)


(define (pipeline-wait/all pline)
  (for ([m (pipeline-members pline)])
    (pipeline-member-wait m)))
(define (pipeline-wait/end pline)
  (pipeline-member-wait (car (reverse (pipeline-members pline)))))
(define (pipeline-wait pline)
  (if (pipeline-status-all? pline)
      (pipeline-wait/all pline)
      (pipeline-wait/end pline)))
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
         [kill-flag (pipeline-end-exit-flag pipeline-spec)]
         [status-all? (pipeline-status-all? pipeline-spec)]
         [bg? (pipeline-start-bg? pipeline-spec)]
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
         [pline (pipeline to-ret from-ret err-outs sanitized #f kill-flag bg? status-all?)]
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
                                     (λ (exn)
                                       (set-box! err-box exn)
                                       (eprintf "~a~n" (exn->string exn)))])
                                   (let ([thread-ret {(pipeline-member-thread m)}])
                                     (set-box! ret-box thread-ret)))
                                 (close-input-port (current-input-port))
                                 (close-output-port (current-output-port))
                                 (close-output-port (current-error-port)))))]
                    [ret-member (pipeline-member to-ret from-ret err-ret
                                                 #f ret-thread ret-box err-box
                                                 #f)])
               (cons ret-member m-outs))))])))
  (define r2-members (reverse r2-members-rev))
  r2-members)

(define (make-pipeline-member-spec s-exp-or-thunk err)
  (if (procedure? s-exp-or-thunk)
      (pipeline-member #f #f err #f s-exp-or-thunk #f #f #t)
      (pipeline-member #f #f err s-exp-or-thunk #f #f #f #t)))
(define (make-pipeline-spec #:in [in #f]
                            #:out [out #f]
                            #:end-exit-flag [end-exit-flag #t]
                            #:status-all? [status-all? #f]
                            #:background? [bg? #f]
                            members err-specs)
  (pipeline in out #f
            (map make-pipeline-member-spec members err-specs)
            #t end-exit-flag bg? status-all?))


(begin-for-syntax
  (define-splicing-syntax-class pipeline-member
    (pattern (~seq (~datum #:with-err) (member:expr err:expr)))
    (pattern member:expr #:attr err #'(current-error-port)))
  (define-splicing-syntax-class in-term
    (pattern (~seq (~datum #:in) term:expr))
    (pattern (~seq) #:attr term #'(current-input-port)))
  (define-splicing-syntax-class out-term
    (pattern (~seq (~datum #:out) term:expr))
    (pattern (~seq) #:attr term #'#f))
  (define-splicing-syntax-class kill-term
    (pattern (~seq (~datum #:kill-at-end) term:expr))
    (pattern (~seq) #:attr term #'#t))
  (define-splicing-syntax-class background-term
    (pattern (~seq (~datum #:background) term:expr))
    (pattern (~seq) #:attr term #'#f))
  (define-splicing-syntax-class end-attrs
    (pattern (~seq (~or (~optional (~seq (~datum #:out) out:expr))
                        (~optional (~seq (~datum #:kill-at-end) kill:expr))
                        (~optional (~seq (~datum #:background) bg:expr)))
                   ...)))
  )

(define-syntax (rash-pipeline-spec stx)
  (syntax-parse stx
    [(rash-pipeline-spec
      in:in-term
      pm:pipeline-member ...+
      ea:end-attrs)
     (with-syntax* ([out (or (attribute ea.out) #'(current-output-port))]
                    [kill (or (attribute ea.kill) #'#t)]
                    [bg (or (attribute ea.bg) #'#f)])
       #'(make-pipeline-spec #:in in.term
                             (list pm.member ...)
                             (list pm.err ...)
                             #:out out
                             #:end-exit-flag kill
                             #:background? bg))]))

(define-syntax (rash-pipeline stx)
  (syntax-parse stx
    [(rash-pipeline arg ...+)
     #'(let ([pline (run-pipeline (rash-pipeline-spec arg ...))])
         (if (pipeline-start-bg? pline)
             pline
             (begin
               (pipeline-wait pline)
               (pipeline-status pline))))]))

(define-syntax (rash-pipeline/funcify stx)
  (syntax-parse stx
    [(r-p/f arg ...+)
     #'(let ([spec (rash-pipeline-spec arg ...)])
         (run-pipeline/funcify spec))]))

(define (run-pipeline/funcify pspec)
  #;(when (not (pipeline-end-exit-flag pspec))
    (error 'run-pipeline/funcify
           "Only pipelines set to kill when the end member finishes are eligible to be funcified."))
  (when (pipeline-start-bg? pspec)
    (error 'run-pipeline/funcify
           "Background pipelines are not eligible to be funcified."))
  (let* ([out (open-output-string)]
         [err (open-output-string)]
         [in (open-input-string "")]
         [pline (parameterize ([current-output-port out]
                               [current-error-port err]
                               [current-input-port in])
                  (run-pipeline pspec))]
         [wait (pipeline-wait pline)]
         [kill (pipeline-kill pline)]
         [status (pipeline-status pline)])
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


  (rash-pipeline '(ls -l /dev) (my-grep "uucp"))

  #;(rash-pipeline #:in #f '(ls) #:with-err ('(grep something) 'stdout)
                 #:background #f #:out #f)
  (rash-pipeline/funcify '(ls) #:with-err ('(grep shell) 'stdout))
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


