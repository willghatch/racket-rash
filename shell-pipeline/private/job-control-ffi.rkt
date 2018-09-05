#lang racket/base

(provide
 job-control-available?
 getpgid
 ;tcsetpgrp
 set-controlling-process-group
 send-sigcont-to-process-group
 )

(require
 ffi/unsafe
 )

(define os (system-type 'os))

(define job-control-available?
  (not (equal? os 'windows)))

(define no-job-control-func (λ _ (error 'rash "job control unavailable")))
(define no-jc! (λ ()
                 (set! job-control-available? #f)
                 no-job-control-func))

(define getpgid
  (get-ffi-obj "getpgid"
               ;; getpgid is in unistd.h
               #f
               (_fun (pid : _int)
                     -> (r : _int)
                     -> (if (< r 0)
                            (error 'rash "getpgid failed")
                            r))
               no-jc!))

(define tcsetpgrp
  (get-ffi-obj "tcsetpgrp"
               ;; tcsetpgrp is in unistd.h
               #f
               (_fun (terminal-fd : _int)
                     (pid : _int)
                     -> (r : _int)
                     -> (if (equal? r 0)
                            (void)
                            (error 'rash "tcsetpgrp failed")))
               no-jc!))

;;; TODO - This is wrong.
;;; I should figure out exactly which file descriptor goes to the terminal
;;; and use it.  But I'm not sure how to do that reliably, so for now I'll
;;; just try the ports for both input and output and consider it successful
;;; if any of them work.
;;; This could go terribly wrong.
(define (set-controlling-process-group ports pgid)
  (define success?
    (for/or ([p ports])
      (define fd (and (file-stream-port? p) (port-file-identity p)))
      (with-handlers ([(λ(e)#t)(λ(e)#f)])
        (tcsetpgrp fd pgid)
        #t)))
  (when (not success?)
    (error 'rash "setting terminal controlling process group failed")))

(define kill
  (get-ffi-obj "kill"
               ;; kill is in signal.h
               #f
               (_fun (pid : _int)
                     (signal : _int)
                     -> (r : _int)
                     -> (if (equal? r 0)
                            (void)
                            (error 'rash "kill (send signal) failed")))
               no-jc!))

(define (kill-process-group pgid signal-number)
  (kill (- pgid) signal-number))

;; My `man 7 signal` lists SIGCONT as 19,18,25... hmmm... may be more system-dependent.
(define SIGCONT 19)

(define (send-sigcont-to-process-group pgid)
  (kill-process-group pgid SIGCONT))

