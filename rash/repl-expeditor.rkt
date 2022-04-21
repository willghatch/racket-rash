#lang racket/base
;; The parameterization doesn't seem to work if I dynamically require expeditor.
;; So I'll just make a dedicated module for the expeditor version of the repl.
;; This may mean that I'm doing something wrong... a quick look at the xrepl
;; implementation shows me that it dynamically requires expeditor... but it
;; doesn't use `call-with-expeditor`.  Maybe if I do the open/read/close stuff
;; like xrepl I can get it to work.
;;
;; Anyway, I would like to get expeditor mode working as a flag to the normal
;; repl module.
;;
;; ALSO - this is silently failing in weird ways when my TERM variable is set to
;; something that it doesn't recognize.  This definitely isn't ready for prime time.
;; More specifically, it still seems to run the rash-repl function, but without
;; properly parameterizing the read function, which then returns a single syntax
;; object read with racket's default reader instead of a list of syntax objects
;; read with the linea reader.  I'm not sure how it's getting to that point and
;; not throwing an exception or something, though.
(require (submod "repl.rkt" _out-for-expeditor)
         expeditor
         racket/port
         racket/string
         )

(expeditor-configure)
(call-with-expeditor
 (λ (e-read)
   (define orig-indenter (current-expeditor-indenter))
   (parameterize ([current-expeditor-reader (λ (in) (port->list repl-read in))]
                  ;[current-expeditor-color-enabled #t]
                  [current-expeditor-ready-checker
                   (λ (in)
                     (with-handlers ([(λ (e) #t) (λ (e) #f)])
                       (define contents (port->string in))
                       (define stripped (string-trim contents))
                       (define re-in (open-input-string contents))
                       (define vals
                         (with-handlers ([(λ (e) #t) (λ (e) #f)])
                           (port->list repl-read re-in)))
                       (and (not (equal? "" stripped)) vals)))]
                  ;; The default indenter crashed on me, so I don't trust it.
                  [current-expeditor-indenter (λ args
                                                (with-handlers
                                                  ([(λ (e) #t) (λ (e) #f)])
                                                  (apply orig-indenter args)))]
                  ;; Whatever the default expeditor lexer is gives me something
                  ;; useful, while the syntax-color/default-lexer doesn't.
                  ;[current-expeditor-lexer
                  ; (dynamic-require 'syntax-color/default-lexer
                  ;                  'default-lexer)]
                  ;; This doesn't seem to do anything.
                  ;[current-expeditor-parentheses
                  ; (map (λ (x) (map string->symbol x))
                  ;      '(("(" ")")
                  ;        ("[" "]")
                  ;        ("{" "}")
                  ;        ("«" "»")
                  ;        ("“" "”")
                  ;        ("◸" "◹")
                  ;        ("◺" "◿")
                  ;        ("◤" "◥")
                  ;        ("◣" "◢")
                  ;        ))]
                  )
     ;; TODO - the expeditor implementation includes a current-expeditor-completer, but it's not exported.
     (rash-repl (void) 0 #f e-read))))
