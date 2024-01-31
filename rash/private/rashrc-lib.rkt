#lang racket/base

;; TODO - This is basically full of quick hacks to get a semi-decent repl.
;;        Everything in this file should be replaced.

(provide
 complete-paths
 complete-commands
 complete-namespaced
 make-composite-completer
 cwd-hack-box

 basic-prompt
 lame-prompt

 current-prompt-function
 current-rash-top-level-print-formatter
 )

(require
 racket/date
 shell/mixed-pipeline
 shell/utils/bourne-expansion-utils
 (prefix-in sp- shell/pipeline)
 "top-level-print.rkt"
 "../prompt-helpers/git-info.rkt"
 racket/list
 racket/string
 readline/pread
 readline/readline
 )

(require (for-syntax racket/base racket/string))
(define-syntax (fail-if-not-6.12+ stx)
  (define vns (string-split (version) "."))
  (if (or (> (string->number (car vns)) 6)
          (and (equal? (string->number (car vns)) 6)
               (>= (string->number (cadr vns)) 12)))
      #'(void)
      (raise-syntax-error 'rash "The Rash REPL can't be used with a Racket version below 6.12")))
(fail-if-not-6.12+)

;; Somehow completions are getting a different current directory.
;; Maybe they run in a different thread sometimes?
;; Let's hack around that.
(define cwd-hack-box (box (current-directory)))

(define (make-composite-completer . completers)
  (λ (pat)
    (flatten
     (for/list ([c completers])
       (c pat)))))


(define (complete-namespaced pat)
  (with-handlers ([(λ _ #t) (λ (e) '())])
    (define names (map symbol->string (namespace-mapped-symbols)))
    (define qpat (string-append "^" (regexp-quote pat)))
    (filter (λ (n) (regexp-match qpat n)) names)))

(define (complete-commands pat)
  (with-handlers ([(λ _ #t) (λ (e) (eprintf "exn: ~a\n" e)'())])
    (define path (getenv "PATH"))
    (define path-parts (string-split path ":"))
    (define files-on-path (flatten
                           (map (λ (d) (with-handlers ([(λ _ #t) (λ (e) '())])
                                         (map (λ (p) (build-path d p))
                                              (directory-list d))))
                                path-parts)))
    (define commands (filter (λ (x) (with-handlers ([(λ _ #t) (λ (e) #f)])
                                      (and (file-exists? x)
                                           (member
                                            'execute
                                            (file-or-directory-permissions x)))))
                             files-on-path))
    (define (basename p)
      (path->string (car (reverse (explode-path p)))))
    (define command-basenames (map basename commands))
    (define qpat (string-append "^" (regexp-quote pat)))
    (filter (λ (n) (regexp-match qpat n)) command-basenames)))

(define (complete-paths pstr)
  (set-completion-append-character! #\null)
  (with-handlers ([(λ _ #t) (λ e '())])
    (parameterize ([current-directory (unbox cwd-hack-box)])
      (let* ([cdir (current-directory)]
             ;; TODO - this isn't expanding $VARIABLES, apparently
             ;; because the $ character is treated as a delimiter
             ;; in libreadline.
             [given-path/expanded (dollar-expand-dynamic pstr)]
             [given-path (string->path given-path/expanded)]
             [given-path-parts (explode-path given-path)]
             [parts-but-last (drop-right given-path-parts 1)]
             [last-part (car (reverse given-path-parts))]
             [build (cond [(equal? pstr "/") (build-path "/")]
                          [(absolute-path? given-path)
                           (apply build-path parts-but-last)]
                          [else (apply build-path cdir parts-but-last)])]
             [build (if (absolute-path? given-path)
                        (if (equal? pstr "/")
                            (build-path "/")
                            (apply build-path parts-but-last))
                        (apply build-path cdir parts-but-last))]
             [listing (directory-list build)]
             [possibles (filter (λ (p) (and (string-prefix? (path->string p)
                                                            (path->string last-part))))
                                listing)]
             [possibles (map (λ (p) (apply build-path
                                           (append parts-but-last (list p))))
                             possibles)]
             [dir-exists-possibles (if (or (directory-exists? pstr)
                                           (equal? pstr ""))
                                       (map (λ (p) (build-path pstr p))
                                            (directory-list pstr))
                                       '())])

        (map (λ (p) (if (directory-exists? p)
                        (string-append p "/")
                        p))
             (map path->string (append possibles dir-exists-possibles)))))))

(define (print-ret-maybe last-ret ret-number)
  (let ([str ((current-rash-top-level-print-formatter) last-ret)])
    (when (not (equal? str ""))
      (printf "Result ~a:\n~a\n" ret-number str))))


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
  (define info (git-info))
  (if info
      (let ([branch (hash-ref info 'branch "?")]
            [ahead (hash-ref info 'ahead 0)]
            [behind (hash-ref info 'behind 0)]
            [dirty (hash-ref info 'dirty? #f)]
            [sub-dirty (hash-ref info 'submodule-dirty? #f)]
            [untracked (hash-ref info 'untracked? #f)]
            [remote-tracking? (hash-ref info 'remote-tracking? #f)]
            [timeout (hash-ref info 'timeout? #t)])
        (string-append
         (default-style "[")
         branch
         (if (equal? 0 ahead)
             ""
             (format "~a~a"
                     (default-style " ▲")
                     (cyan (if (number? ahead) ahead "?"))))
         (if (equal? 0 behind)
             ""
             (format "~a~a" (default-style " ▼")
                     (cyan (if (number? behind) behind "?"))))
         (if (eq? #t dirty) (red " D") "")
         (if (eq? #t sub-dirty) (red " S") "")
         (if (eq? #t untracked) (red " U") "")
         (if (eq? #f remote-tracking?) (default-style " N") "")
         (if (eq? #t timeout) (red " Time-out") "")
         (default-style "] ")))
      ""))

;; TODO - add path coloring like in megaprompt, maybe with some more info and color options
;; TODO - add path shortening to a maximum length, or more generally finding the
;; max length a prompt string should be and adjusting all parts to it...

(define (basic-prompt #:last-return-value [last-ret #f]
                      #:last-return-index [last-ret-n 0])
  (when (> last-ret-n 0)
    (print-ret-maybe last-ret last-ret-n))
  (let* ([cdate (current-date)]
         [chour (date-hour cdate)]
         [cmin (date-minute cdate)]
         [padded-min (if (< cmin 10)
                         (string-append "0" (number->string cmin))
                         cmin)])
    (printf "~a:~a ~a~a~a\n"
            (cyan chour) padded-min
            (with-handlers ([(λ _ #t) (λ (e) (default-style "[git-info-error] "))])
              (git-info-with-style))
            (bblue (path->string (current-directory)))
            (default-style ""))
    ;(current-prompt (string->bytes/utf-8 "➤ "))
    (readline-prompt (string->bytes/utf-8 "> "))
    ))

(define (lame-prompt #:last-return-value [last-ret #f]
                     #:last-return-index [last-ret-n #f])
  (print-ret-maybe last-ret last-ret-n)
  ;(printf ">")
  (readline-prompt #"> ")
  )

(define current-prompt-function (make-parameter (if windows?
                                                    lame-prompt
                                                    basic-prompt)))
