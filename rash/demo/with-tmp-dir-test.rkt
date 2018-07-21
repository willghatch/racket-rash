#lang rash
(require "with-tmp-dir.rkt")

(define dir #f)
with-tmp-dir d {
                echo $d
                echo hello foo bar &> $d/file
                echo $d/file
                cat $d/file
                (set! dir d)
}

(when (directory-exists? dir)
  (error "Apparently the directory still exists.  Failed test 1."))

in-tmp-dir {
            (set! dir (current-directory))
            echo in (current-directory)
            }

(when (directory-exists? dir)
  (error "failed test 2"))
