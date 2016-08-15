#lang rash

& @(define my-flag '-l)
cat /etc/hostname ;; testing that this is a comment
ls @my-flag /dev | grep uucp
uname -a | wc
