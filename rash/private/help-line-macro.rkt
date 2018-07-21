#lang rash
(provide help)
(require
 (for-syntax
  racket/base
  syntax/parse))

(define-line-macro help
  (syntax-parser
    (_ #'(begin
           (printf "This is a help macro!\n")
           (printf "It should be helpful, but currently isn't.\n")
           (printf "I want it to eventually check if things that are asked about are bound and then link automatically to the docs (in a browser configured by the user -- including possibly some text-mode browser), and if things are not bound automatically search for the man page or info page.  Maybe it could have some key words or flag arguments to make it run in different modes.  Modes could include a shell-programming tutorial, overviews of various Rash concepts, etc.  Maybe these should just link to web pages...  Anyway, this macro should be helpful.  But right now it's not.  Try running `raco docs` or something instead.\n\n")
           ))))
