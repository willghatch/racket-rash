#lang racket/base

#|
Stuff to give quick demos.  Eventually most of this should be cleaned up and some set of good things should be provided by default.
|#


(provide
 (all-defined-out)
 (all-from-out shell/demo/more-pipeline-operators)
 (all-from-out racket/string)
 (all-from-out racket/port)
 (all-from-out racket/function)
 (all-from-out file/glob)
 (for-syntax
  (all-from-out racket/base)
  (all-from-out racket/syntax)
  (all-from-out syntax/parse)
  ))


(require
 rash

 ;; These are the interesting demo files to look at for defining pipeline operators.
 shell/demo/more-pipeline-operators

 racket/string
 racket/port
 racket/function
 file/glob
 (for-syntax
  racket/base
  racket/syntax
  syntax/parse
  ))

;; convenient short name for identity line-macro and function
(define-line-macro id
  (syntax-parser
    ;; pass through the normal case
    [(_ e) #'e]
    ;; I guess multiple values makes sense here...
    [(_ e ...) #'(values e ...)]
    ;; And we want it to work as a first-order function.
    [_ #'(λ args (apply values args))]))

(define-syntax #%upper-triangles (make-rename-transformer #'rash))
(define-syntax #%lower-triangles (make-rename-transformer #'rash/wired))

(define-syntax =o= (make-rename-transformer #'=object-pipe=))
(define-syntax =ol= (make-rename-transformer #'=object-pipe/left=))
(define-syntax =oe= (make-rename-transformer #'=object-pipe/expression=))
(define-syntax =u= (make-rename-transformer #'=unix-pipe=))
(define-syntax \|o (make-rename-transformer #'=object-pipe=))
(define-syntax \|e (make-rename-transformer #'=object-pipe/expression=))
(define-syntax \|seq (make-rename-transformer #'=for/list=))
(define-syntax \|>l (make-rename-transformer #'=object-pipe/left=))
(define-syntax \|ou (make-rename-transformer #'=obj-if-def/unix-if-undef=))


(define (highlighting-output-port outer-oport)
    (define-values (from-pipe to-pipe) (make-pipe))
    (thread (λ ()
              (define (loop)
                (let ([oline (read-line from-pipe)])
                  (if (equal? eof oline)
                      (void)
                      (begin
                        (fprintf outer-oport "\033[31m~a\033[0m\n" oline)
                        (loop)))))
              (loop)))
    to-pipe)
(define (pass-through-port outer-oport)
    (define-values (from-pipe to-pipe) (make-pipe))
    (thread (λ () (copy-port from-pipe outer-oport)))
    to-pipe)


(define (grep-func str regex)
  (let ([r (cond [(regexp? regex) regex]
                 [(string? regex) regex]
                 [else (format "~a" regex)])])
    (string-append
     (string-join (filter identity
                          (for/list ([line (string-split str "\n")])
                            (and (regexp-match r line) line)))
                  "\n")
     "\n")))

(require racket/system)
(define-line-macro a
  (λ (stx)
    (syntax-case stx ()
      [(_ arg1 arg2 arg ...)
       #'(system* (find-executable-path (getenv "EDITOR")) 'arg1 'arg2 'arg ...)]
      [(_ arg)
       #'(let ([pstr (format "~a" 'arg)])
           (if (directory-exists? pstr)
               (current-directory pstr)
               (system* (find-executable-path (getenv "EDITOR")) pstr)))])))

(define-line-macro def
  (λ (stx)
    (syntax-parse stx
      [(_ name:id line-arg ...+)
       #'(define name (#%linea-line line-arg ...))]
      [(_ (name:id fp:id ...) line-arg ...+)
       #'(define (name fp ...) (#%linea-line line-arg ...))]
      ;; let's be generous, but not good at catching errors
      [(_ name-or-func-form line-arg ...+)
       #'(define name-or-func-form (#%linea-line line-arg ...))])))
(define-line-macro rash-lambda
  (syntax-parser
    [(_ (fp ...) line-arg ...+)
     #'(lambda (fp ...) (#%linea-line line-arg ...))]))


(define-simple-pipeline-alias d 'ls '--color=auto)
(define-simple-pipeline-alias di 'ls '-laFh '--color=auto)
(define-simple-pipeline-alias gc "git" 'commit )
(define-simple-pipeline-alias gs "git" 'status )
(define-simple-pipeline-alias gd "git" 'diff )
(define-simple-pipeline-alias gka "gitk" '--all )
(define-simple-pipeline-alias gta "tig" '--all )
(define-simple-pipeline-alias greb "git" 'rebase )
(define-simple-pipeline-alias gru "git" 'remote 'update )
(define-simple-pipeline-alias gunadd "git" 'reset 'HEAD )
(define-simple-pipeline-alias gco "git" 'checkout )
(define-simple-pipeline-alias gcob "git" 'checkout '-b )
(define-simple-pipeline-alias gclone "git" 'clone '--recursive )
(define-simple-pipeline-alias gp "git" 'push )
(define-simple-pipeline-alias ga "git" 'add )

(define-pipeline-alias my-grep
  (syntax-parser
   [(_ pat) #'(=object-pipe= grep-func current-pipeline-argument pat)]))

;; note that grep returns 1 when it finds nothing, which is normally considered an error
(define-simple-pipeline-alias grep "grep" #:success (list 1))



