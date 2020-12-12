#lang racket/base

(provide closure-substitution)

(require
 shell/pipeline
 basedir
 (submod "substitutions.rkt" random-name-for-temporary-file)
 racket/unix-socket
 racket/file
 )

(define (closure-substitution procedure #:read-stdin? [read-stdin? #f])
  (shell-substitution
   (λ ()
     (define pipeline-done-semaphore (make-semaphore))
     (define socket-path (writable-runtime-file (random-name-for-temporary-file)))
     (define script-file (writable-runtime-file (random-name-for-temporary-file)))
     (make-parent-directory* socket-path)
     (define listener (unix-socket-listen socket-path))

     (define worker-thread
       (thread
        (λ ()
          (let loop ()
            (define sunk
              (sync pipeline-done-semaphore (unix-socket-accept-evt listener)))
            (if (eq? sunk pipeline-done-semaphore)
                (begin (unix-socket-close-listener listener)
                       (delete-file socket-path))
                (let ([in (car sunk)]
                      [out (cadr sunk)])
                  (with-handlers
                    ([(λ (e) #t)
                      ;; TODO - I'm not really sure what to do with exceptions here.
                      (λ (e)
                        (eprintf "error in closure-substitution: ~v\n" e))])
                    (define args (read in))
                    (define newline (read-char in))
                    (parameterize ([current-input-port in]
                                   [current-output-port out])
                      (apply procedure args)))
                  (flush-output out)
                  (close-output-port out)
                  (close-input-port in)
                  (loop)))))))

     (with-output-to-file script-file
       (λ ()
         (define this-racket-path
           ;; TODO - I can't guarantee the original environment here, including PATH.  So this isn't great.
           (parameterize ([current-directory (find-system-path 'orig-dir)])
             (find-executable-path (find-system-path 'exec-file))))
         (printf "#!~a\n" this-racket-path)
         (printf "#lang racket/base\n")
         (writeln `(begin
                     (require racket/unix-socket racket/port)
                     (define-values (in out)
                       (unix-socket-connect ,(path->string socket-path)))
                     (writeln (vector->list (current-command-line-arguments))
                              out)
                     (flush-output out)
                     (define out-thread
                       (when ,read-stdin?
                         (thread (λ ()
                                   (copy-port (current-input-port) out)
                                   (close-output-port out)))))
                     (define in-thread
                       (thread (λ ()
                                 (copy-port in (current-output-port))
                                 (close-input-port in))))
                     (thread-wait in-thread)
                     (when ,read-stdin?
                       (thread-wait out-thread))
                     (exit 0)))))
     (file-or-directory-permissions script-file #o700)

     (hash 'argument script-file
           'pipeline-done-procedure (λ (pline)
                                      (semaphore-post pipeline-done-semaphore)
                                      (delete-file script-file))))))
