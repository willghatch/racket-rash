#lang rash

(require (for-syntax racket/base))

(define-syntax =o= (make-rename-transformer #'=basic-object-pipe=))
(define-syntax =u= (make-rename-transformer #'=crappy-basic-unix-pipe=))

=u= echo testing 123 =u= grep test
(printf "here~n")
=o= eprintf "here~n"
(default-pipe-starter! =u=)
echo testing 123 =u= grep test
(default-pipe-starter! =o=)
eprintf "testing eprintf with =o=~n"
;=u= echo testing 123 =u= grep test =o= string-split
(require racket/string)
(define my-flag '-l)
=u= cat /etc/hostname ;; testing that this is a comment
;ls $my-flag /dev | grep uucp
;echo $(rash/number «uname -a | wc $my-flag»)
