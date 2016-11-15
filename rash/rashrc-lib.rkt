#lang racket/base

(provide current-prompt-function)

(require racket/date)
(require shell/pipeline)


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

(define (basic-prompt #:last-return-value [last-ret #f]
                      #:last-return-index [last-ret-n #f])
  (let* ([cdate (current-date)]
         [chour (date-hour cdate)]
         [cmin (date-minute cdate)]
         [padded-min (if (< cmin 10)
                         (string-append "0" (number->string cmin))
                         cmin)]
         [ret-show (green (format-ret last-ret))])
    (printf "~a:~a ~a ~a~a ~a~a~n~a "
            (cyan chour) padded-min
            (bblue (path->string (current-directory)))
            (default-style "｢R") last-ret-n ret-show (default-style "｣")
            (default-style (if (equal? (system-type 'os) 'windows) ">" "➤")))))

(define current-prompt-function (make-parameter basic-prompt))
