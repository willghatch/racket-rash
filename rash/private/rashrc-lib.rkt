#lang racket/base

(provide current-prompt-function)

(require racket/date)
(require shell/pipeline)
(require "rashrc-git-stuff.rkt")


(define (format-ret last-ret)
  (cond [(exn? last-ret)
         (begin (eprintf "~a~n" last-ret)
                ;; let any filtering output finish
                (sleep 0.01)
                (red "exn!"))]
        [(pipeline? last-ret)
         ({if (pipeline-success? last-ret)
              green
              red}
          ;(format-ret (pipeline-status last-ret))
          (format "~s" last-ret)
          )]
        [else (format "~s" last-ret)]))

;; TODO - use a library for these functions?
;;        Or do I not want another dependency?
(define (mstyle n)
  (λ (s) (format "\033[~am~a" n s)))
(define (mstyle2 n1 n2)
  (λ (s) (format "\033[~a;~am~a" n1 n2 s)))
(define default-style (mstyle 0))
(define cyan (mstyle 36))
(define red (mstyle 31))
(define green (mstyle 32))
(define bblue (mstyle2 1 34))

(define windows? (equal? (system-type 'os) 'windows))

(define (git-info-with-style)
  #|
  TODO - getting git info may be slow depending on file system.
  Eg. network mounts or busy file systems may make git info queries
  unusably slow, or huge git histories may slow this down.  I should
  add some sort of timeout, or only give information that comes out
  quickly.
  |#
  (define info (get-git-info))
  (if info
      (string-append
       (default-style "[")
       (hash-ref info 'branch) " "
       (if (< 0 (hash-ref info 'ahead))
           (format "~a~a" (default-style "▲") (cyan (hash-ref info 'ahead)))
           "")
       (if (< 0 (hash-ref info 'behind))
           (format "~a~a" (default-style "▼") (cyan (hash-ref info 'behind)))
           "")
       (if (hash-ref info 'dirty?) (red " D") "")
       (if (hash-ref info 'submodule-dirty?) (red " S") "")
       (if (hash-ref info 'untracked?) (red " U") "")
       (default-style "] "))
      ""))

;; TODO - add path coloring like in megaprompt, maybe with some more info and color options
;; TODO - add path shortening to a maximum length, or more generally finding the
;; max length a prompt string should be and adjusting all parts to it...

(define (basic-prompt #:last-return-value [last-ret #f]
                      #:last-return-index [last-ret-n #f])
  (let* ([cdate (current-date)]
         [chour (date-hour cdate)]
         [cmin (date-minute cdate)]
         [padded-min (if (< cmin 10)
                         (string-append "0" (number->string cmin))
                         cmin)]
         [ret-show (green (format-ret last-ret))])
    (printf "~a:~a ~a~a~a~n~a "
            (cyan chour) padded-min
            (git-info-with-style)
            (bblue (path->string (current-directory)))
            (if (and last-ret (< 0 last-ret-n))
                (format " ~a~a ~a~a"
                        (default-style "｢R")
                        last-ret-n
                        ret-show
                        (default-style "｣"))
                "")
            (default-style "➤"))))

(define (lame-prompt #:last-return-value [last-ret #f]
                     #:last-return-index [last-ret-n #f])
  (when (and last-ret (< 0 last-ret-n))
    (printf "[~a ~a~a~n" last-ret-n (format-ret last-ret) (default-style "]")))
  (printf ">"))

(define current-prompt-function (make-parameter (if windows?
                                                    lame-prompt
                                                    basic-prompt)))
