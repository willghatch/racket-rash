#lang racket/base

(require "shell-funcs.rkt")
(require racket/string)
(require racket/function)

#|
TODO
write real tests that are reproducible independent of the machine.
Put ones that don't access the file system (don't run subprocesses)
into a test submodule.
|#


(define (grep-func str regex)
  (string-append
   (string-join (filter identity
                        (for/list ([line (string-split str "\n")])
                          (and (regexp-match regex line) line)))
                "\n")
   "\n"))
(define my-grep (shellify grep-func))


;(run-pipeline '(ls -l /dev) `(,my-grep "uucp"))
(run-pipeline '(ls -l /dev) `(grep "uucp"))
(define d (alias-func (λ args (list* 'ls '--color=always args))))
(current-shell-functions
 (hash "ls" (alias-func (λ args (list* 'process 'ls '--color=auto args)))))
;(run-pipeline `(,d -l /dev))
(run-pipeline `(ls -l /dev))

(run-pipeline/funcify `(ls) (pipeline-member-spec '(grep shell) 'stdout))

