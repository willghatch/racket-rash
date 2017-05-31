#lang rash

(require (for-syntax racket/base))

(define-syntax =o= (make-rename-transformer #'=object-pipe=))
(define-syntax =u= (make-rename-transformer #'=crappy-basic-unix-pipe=))

=u= echo testing 123 =u= grep test =o= string-upcase =basic-object-pipe= list 1 2 3
=u= echo testing 123 =u= grep test =o= string-upcase =basic-object-pipe= list 1 current-rash-pipeline-argument 2 3

=u= echo testing 123 =o= string-upcase =basic-object-pipe= list 1 (rash «=o= list 5 =basic-object-pipe= list 11 22 current-rash-pipeline-argument 33») 2 current-rash-pipeline-argument 3
;=u= echo testing 123 =u= grep test =o= string-upcase =u= grep aoeu

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
