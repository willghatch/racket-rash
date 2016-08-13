#lang rash

cat /etc/hostname ;; testing that this is a comment
ls @(if #t '-l '-a) /dev | grep uucp
uname -a | wc
