#lang racket/base

#|
Good info on job control is here:
https://www.gnu.org/software/libc/manual/html_node/Implementing-a-Shell.html
|#

(provide
 job-control-available?
 initialize-job-control!
 job-control-initialized?
 getpgid
 waitpid-wrap
 ;tcsetpgrp
 set-terminal-controlling-process-group!
 return-terminal-control!
 send-sigcont-to-process-group!
 )
(module+ repl
  (provide initialize-job-control!))

(require
 ffi/unsafe
 racket/match
 )

(define os (system-type 'os))

(define job-control-available?
  (not (equal? os 'windows)))

(define -job-control-initialized? #f)
(define (job-control-initialized?) -job-control-initialized?)
(define my-terminal-fd #f)

(define no-job-control-func (λ _ (error 'rash "job control unavailable")))
(define no-jc! (λ ()
                 (set! job-control-available? #f)
                 no-job-control-func))

(define getpid
  (get-ffi-obj "getpid"
               #f
               (_fun -> (r : _int))))

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
(define setpgid
  (get-ffi-obj "setpgid"
               #f
               (_fun (pid : _int)
                     (pgid : _int)
                     -> (r : _int)
                     -> (if (equal? 0 r)
                            (void)
                            (error 'rash "setpgid failed")))))

(define scheme-get-port-fd
  (get-ffi-obj "scheme_get_port_fd"
               #f
               (_fun (port : _racket)
                     -> (r : _int)
                     -> (if (< r 0)
                            (error 'rash
                                   "internal error -- bad call to scheme_get_port_fd")
                            r))))

(define isatty
  (get-ffi-obj "isatty"
               #f
               (_fun (fd : _int)
                     -> (r : _int)
                     -> (equal? r 1))))

(define (ports->terminal-fd ports)
  (for/or ([p ports])
    (with-handlers ([(λ(e)#t)(λ(e)#f)])
      (let ([fd (and (file-stream-port? p) (scheme-get-port-fd p))])
        (and (isatty fd) fd)))))

(define tcgetpgrp
  (get-ffi-obj "tcgetpgrp"
               ;; tcgetpgrp is in unistd.h
               #f
               (_fun (terminal-fd : _int)
                     -> (r : _int)
                     -> (if (< r 0)
                            (error 'rash "tcgetpgrp failed")
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
                            (let ([err saved-errno])
                              (error 'rash "tcsetpgrp failed"))))
               no-jc!))

(define (set-terminal-controlling-process-group! pgid)
  (define fd my-terminal-fd)
  (define success?
    (and fd
         (with-handlers ([(λ(e)#t)(λ(e)
                                    (println e)
                                    #f)])
           (tcsetpgrp fd pgid)
           #t)))
  (when (not success?)
    (error 'rash "setting terminal controlling process group failed")))

(define (return-terminal-control!)
  (set-terminal-controlling-process-group! (getpgid 0)))

(define (am-i-the-controlling-process-group? fd)
  (define terminal-group
    (and fd
         (with-handlers ([(λ(e)#t)(λ(e)#f)])
           (tcgetpgrp fd))))
  (when (not terminal-group)
    (error 'rash "can't get controlling terminal info"))
  (define my-group
    (getpgid 0))
  (equal? my-group terminal-group))

(define (loop-until-foreground terminal-fd)
  (with-handlers ([(λ(e)#t) (λ(e)#f)])
    (let loop ()
      (if (am-i-the-controlling-process-group? terminal-fd)
          #t
          (begin
            (kill (- (getpgid 0))
                  SIGTTIN)
            (loop))))))

;; TODO - these are the values on NixOS today, but I should get these from headers somehow...
(define CLD_STOPPED 5)
(define CLD_CONTINUED 6)

(define waitpid
  (get-ffi-obj "waitpid"
               ;; in sys/wait.h
               #f
               (_fun (pid : _int)
                     (wstatus : (_ptr o _int))
                     (options : _int)
                     -> (child-with-new-status/r : _int)
                     -> (if (< child-with-new-status/r 0)
                            (error 'rash "waitpid failed")
                            (match child-with-new-status/r
                              [CLD_STOPPED 'stopped]
                              [CLD_CONTINUED 'continued]
                              [else 'dead])))))
(define (waitpid-wrap pid)
  (waitpid pid 0))

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

;; TODO
;; I need a good way to get signal numbers from system headers...
;; These are just taken from `man 7 signal` for x86/ARM/most others...
(define SIGINT 2)
(define SIGQUIT 3)
(define SIGCHLD 17)
(define SIGCONT 18)
(define SIGSTOP 19)
(define SIGTSTP 20)
(define SIGTTIN 21)
(define SIGTTOU 22)


(define (send-sigcont-to-process-group! pgid)
  (kill-process-group pgid SIGCONT))


;; These are signal handler values
(define SIG_ERR -1)
(define SIG_DFL 0)
(define SIG_IGN 1)

(define signal
  (get-ffi-obj "signal"
               ;; in signal.h
               #f
               (_fun (signum : _int)
                     (handler : _int)
                     ->
                     (oldhandler : _int)
                     ->
                     (if (equal? oldhandler SIG_ERR)
                         (error 'rash "job control signal masking error")
                         oldhandler))))

(define (initialize-job-control! ports)
  ;; This series of steps is basically taken from:
  ;; https://www.gnu.org/software/libc/manual/html_node/Initializing-the-Shell.html#Initializing-the-Shell
  (define success
    (with-handlers ([(λ(e)#t)(λ(e)#f)])
      (define terminal-fd (ports->terminal-fd ports))
      (set! my-terminal-fd terminal-fd)
      (loop-until-foreground terminal-fd)

      ;; TODO - Which of these signals are used specially in Racket?
      ;;        And what should I do about it?
      ;(signal SIGINT SIG_IGN)
      ;(signal SIGQUIT SIG_IGN)
      (signal SIGTSTP SIG_IGN)
      (signal SIGTTIN SIG_IGN)
      (signal SIGTTOU SIG_IGN)
      ;(signal SIGCHLD SIG_IGN)

      (define shell-pid (getpid))
      ;; make our own process group
      (define setpgid-ret (setpgid shell-pid shell-pid))
      ;; TODO - check errors

      ;; take control of terminal
      (tcsetpgrp terminal-fd shell-pid)

      ;; TODO - save default terminal attributes for shell with tcgetattr

      #t))
  (if success
      (begin
        (set! -job-control-initialized? #t)
        (void))
      (error 'initialize-job-control! "job control initialization failed!")))
