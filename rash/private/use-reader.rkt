#lang rash

echo testing 123
(require racket/string)
(define my-flag '-l)
cat /etc/hostname ;; testing that this is a comment
ls $my-flag /dev | grep uucp
echo $(rash/number «uname -a | wc $my-flag»)
