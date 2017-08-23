#lang racket/base

#|
Stuff to give quick demos.  Eventually most of this should be cleaned up and some set of good things should be provided by default.
|#

;; be sure to put something like this in your file!
;(set-default-pipeline-starter! \|)


(provide
 (all-defined-out)
 (all-from-out shell/demo/define-rash-alias)
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
 shell/demo/define-rash-alias
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

;; convenient short name for identity function
(define (id x) x)

(define-syntax #%upper-triangles (make-rename-transformer #'rash))
(define-syntax #%lower-triangles (make-rename-transformer #'rash/wired))

(define-syntax =o= (make-rename-transformer #'=object-pipe=))
(define-syntax =ol= (make-rename-transformer #'=object-pipe/left=))
(define-syntax =oe= (make-rename-transformer #'=object-pipe/expression=))
(define-syntax =u= (make-rename-transformer #'=quoting-basic-unix-pipe=))
(define-syntax \|> (make-rename-transformer #'=object-pipe=))
(define-syntax \|o (make-rename-transformer #'=object-pipe=))
(define-syntax \|e (make-rename-transformer #'=object-pipe/expression=))
(define-syntax \|seq (make-rename-transformer #'=for/list=))
(define-syntax \|>l (make-rename-transformer #'=object-pipe/left=))
(define-syntax \|u (make-rename-transformer #'=quoting-basic-unix-pipe=))
(define-syntax \|g (make-rename-transformer #'=globbing-basic-unix-pipe=))
(define-syntax \| (make-rename-transformer #'=aliasing-unix-pipe=))
(define-syntax \|ou (make-rename-transformer #'=obj-if-def/unix-if-undef=))
(define-syntax _ (make-rename-transformer #'current-pipeline-argument))


(define (highlighting-output-port outer-oport)
    (define-values (from-pipe to-pipe) (make-pipe))
    (thread (λ ()
              (define (loop)
                (let ([oline (read-line from-pipe)])
                  (if (equal? eof oline)
                      (void)
                      (begin
                        (fprintf outer-oport "\033[31m~a\033[0m~n" oline)
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
       #'(define name (rash-line-or-line-macro line-arg ...))]
      [(_ (name:id fp:id ...) line-arg ...+)
       #'(define (name fp ...) (rash-line-or-line-macro line-arg ...))]
      ;; let's be generous, but not good at catching errors
      [(_ name-or-func-form line-arg ...+)
       #'(define name-or-func-form (rash-line-or-line-macro line-arg ...))])))
(define-line-macro rash-lambda
  (syntax-parser
    [(_ (fp ...) line-arg ...+)
     #'(lambda (fp ...) (rash-line-or-line-macro line-arg ...))]))


(define-simple-rash-alias d 'ls '--color=auto)
(define-simple-rash-alias di 'ls '-l '--color=auto)
(define-simple-rash-alias gc "git" 'commit )
(define-simple-rash-alias gs "git" 'status )
(define-simple-rash-alias gd "git" 'diff )
(define-simple-rash-alias gka "gitk" '--all )
(define-simple-rash-alias gta "tig" '--all )
(define-simple-rash-alias greb "git" 'rebase )
(define-simple-rash-alias gru "git" 'remote 'update )
(define-simple-rash-alias gunadd "git" 'reset 'HEAD )
(define-simple-rash-alias gco "git" 'checkout )
(define-simple-rash-alias gcob "git" 'checkout '-b )
(define-simple-rash-alias gclone "git" 'clone '--recursive )
(define-simple-rash-alias gp "git" 'push )
(define-simple-rash-alias ga "git" 'add )

(define-rash-alias my-grep
  (syntax-parser
   [(_ pat) #'(=object-pipe= grep-func current-pipeline-argument pat)]))

;; note that grep returns 1 when it finds nothing, which is normally considered an error
(define-simple-rash-alias grep "grep" #:success (list 1))

(define-syntax envar
  (syntax-parser
    [(rec x:id)
     #'(rec x #:default
            (error 'envar
                   "unset environment variable with no default: ~a"
                   (symbol->string 'x)))]
    [(_ x:id #:default def:expr)
     #'(or (getenv (symbol->string 'x)) def)]
    [(_ x:id ve:expr)
     #'(let ([v ve])
         (and (or (putenv (symbol->string 'x) v)
                  (error 'envar "setting environment variable failed"))
              v))]))

