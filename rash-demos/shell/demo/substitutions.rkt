#lang racket/base
(provide

 ;; These just attach a port to a temporary named pipe.  A more general core for “process substitution” (IE <() and >() in Bash).
 input-port-to-named-pipe-substitution
 output-port-to-named-pipe-substitution

 ;; These are somewhat generalized implementations of “process substitution”, IE <() and >() from bash.  They use named pipes (not file descriptors of inner pipelines, which some shells do) but allow arbitrary racket code, not just pipelines.  Something more analogous to <() and >() is a simple macro away.  But <() isn't very common, and I'm not sure it needs a particularly terse notation.  >() is crazy, I've never used it, and it probably doesn't work like many people expect since programs that take an output file usually replace rather than append to the file.
 with-output-to-named-pipe-substitution
 with-input-from-named-pipe-substitution
 )

(require
 shell/pipeline
 basedir
 racket/format
 racket/random
 racket/file
 racket/port
 )

#|
What are some good (or at least interesting) substitution ideas?
• “process substitution”, IE sub-pipeline output to named pipe substitution, a la <() and >() in Bash and friends.
• pipeline stdout substitution (a la `$()` in bash, already covered effectively with #{}, though, since it needs no cleanup.)
• closure substitution (idea originally from Alexis King -- give the subprocess the name of a temporary script that opens a socket to a fresh thread in the rash script that executes the given procedure on the argv and stdin, setting `current-output-port` and `current-error-port` to something that communicates over the socket).
• temporary file substitution (IE just make a temporary file, pass the name of that file, then clean it up when the pipeline is over).
• temporary directory substitution.
• temporary file system substitution (IE mount a file system at a temporary directory path, pass that path to the pipeline, umount after pipeline exit.  This could actually be useful given various FUSE programmatic file systems.)
• user/group substitution (IE as root, create a temporary user/group and pass its UID/GID to the process).  Eg. you could pass this to `sudo` to run a command as a fresh user.  I don't know why you would want to do this, though.
• http url substitution using the web server (this one is really dumb, but, hey, it's doable).
• temporary virtual block file substitution
• temporary symlink substitution
• host-name/ip-address substitution (to a temporary virtual machine, maybe?)
• Any other resource that you might pass a reference of to a subprocess.
|#

(define (random-name-for-temporary-file)
  ;; Get a sufficiently random name that collisions should statistically never happen.
  (string->immutable-string
   (format "tmp-file_~a"
           (apply string-append
                  (for/list ([b (crypto-random-bytes 32)])
                    (~r b #:base 16 #:min-width 2 #:pad-string "0"))))))
(define (mk-temp-fifo)
  (define tmp-file (writable-runtime-file (random-name-for-temporary-file)))
  (make-parent-directory* tmp-file)
  (run-subprocess-pipeline `(mkfifo --mode 700 ,tmp-file))
  tmp-file)

(define (input-port-to-named-pipe-substitution ip)
  (shell-substitution
   (λ ()
     (define tmp-file (mk-temp-fifo))
     (define tmp-file-port (open-output-file tmp-file #:exists 'append))
     (thread (λ ()
               (copy-port ip tmp-file-port)
               (close-output-port tmp-file-port)))
     (hash 'argument tmp-file
           'pipeline-done-procedure (λ (pline)
                                      (close-output-port tmp-file-port)
                                      (delete-file tmp-file))))))

(define (output-port-to-named-pipe-substitution op)
  (shell-substitution
   (λ ()
     (define tmp-file (mk-temp-fifo))
     (define tmp-file-port (open-input-file tmp-file))
     (thread (λ ()
               (copy-port tmp-file-port op)
               (close-input-port tmp-file-port)
               (delete-file tmp-file)))
     (hash 'argument tmp-file))))

(define (with-output-to-named-pipe-substitution thunk)
  ;; IE this is like Bash's <(), except it takes a thunk of arbitrary Racket code.
  (shell-substitution
   (λ ()
     (define-values (in out) (make-pipe))
     (thread
      (λ ()
        (parameterize ([current-output-port out])
          (thunk)
          (close-output-port out))))
     (hash 'argument (input-port-to-named-pipe-substitution in)))))

(define (with-input-from-named-pipe-substitution thunk)
  ;; IE this is like Bash's >(), except it takes a thunk of arbitrary Racket code.
  ;; I've never had any use for >(), but here is a link to some uses for logging: http://mywiki.wooledge.org/BashFAQ/106
  (shell-substitution
   (λ ()
     (define-values (in out) (make-pipe))
     (thread
      (λ ()
        (parameterize ([current-input-port in])
          (thunk)
          (close-input-port in))))
     (hash 'argument (output-port-to-named-pipe-substitution out)))))

;; TODO - pipeline input/output substitution where we actually get the path to the file descriptor of stdin/stdout rather than going through a named pipe.
