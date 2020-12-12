#lang racket/base
(provide
 input-port-to-named-pipe-substitution
 with-output-to-named-pipe-substitution
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
What are some good substitution ideas?
• sub-pipeline output to named pipe substitution (IE give the name of the pipe for subprocess to read, a la `<()` in bash).
  • Also with input (a la `>()`), also using temporary files instead of named pipes.
  • Also support named pipe substitution where the pipe is connected to an arbitrary Racket port instead of specifically a pipeline.  Eg. `with-output/input-to-substitution-named-pipe`.
• pipeline stdout substitution (a la `$()` in bash, already covered effectively with #{}, though)
• closure substitution (idea originally from Alexis King -- give the subprocess the name of a temporary script that opens a socket to a fresh thread in the rash script that executes the given procedure on the argv and stdin, setting `current-output-port` and `current-error-port` to something that communicates over the socket).
• temporary file substitution (IE just make a temporary file, pass the name of that file, then clean it up when the pipeline is over).
• temporary directory substitution.
• temporary file system substitution (IE mount a file system at a temporary directory path, pass that path to the pipeline, umount after pipeline exit.  This could actually be useful given various FUSE programmatic file systems.)
• user/group substitution (IE as root, create a temporary user/group and pass its UID/GID to the process)
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

(define (input-port-to-named-pipe-substitution ip)
  (shell-substitution
   (λ ()
     (define tmp-file (writable-runtime-file (random-name-for-temporary-file)))
     (make-parent-directory* tmp-file)
     (run-subprocess-pipeline `(mkfifo --mode 700 ,tmp-file))
     (define tmp-file-port (open-output-file tmp-file #:exists 'append))
     (thread (λ ()
               (copy-port ip tmp-file-port)
               (close-output-port tmp-file-port)))
     (hash 'argument tmp-file
           'pipeline-done-procedure (λ (pline)
                                      (close-output-port tmp-file-port)
                                      (delete-file tmp-file))))))

(define (with-output-to-named-pipe-substitution thunk)
  (shell-substitution
   (λ ()
     (define-values (in out) (make-pipe))
     (thread
      (λ ()
        (parameterize ([current-output-port out])
          (thunk)
          (close-output-port out))))
     (hash 'argument (input-port-to-named-pipe-substitution in)))))

;; TODO - pipeline input/output substitution where we actually get the path to the file descriptor of stdin/stdout rather than going through a named pipe.
