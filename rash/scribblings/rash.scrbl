#lang scribble/manual

@title[#:tag "rash"]{RASH: RAcket SHell Library}
@author+email["William Hatch" "william@hatch.uno"]

@defmodule[rash]
@(require rash)

@section{RASH Guide}

THIS IS ALL ALPHA AND UNSTABLE

tl;dr -- in the string argument of a rash macro and @literal{#lang rash} each line is
wrapped in a pipeline s-expression, and you can escape to racket with
@literal|{$}|.  A line that starts with & is not wrapped to be a
pipeline.  IE. use @literal|{&$(define ...)}|, etc to have racket
definitions and expressions at the top level.
The & thing is a stopgap until I write a better parser.  My plan is to
have lines that start with an open paren be normal racket, otherwise be
shell-ish.

Rash is a language and library for writing shell scripts and including them in
Racket programs.  It is basically a syntax wrapper for the
@other-doc['(lib "shell/shell-pipeline.scrbl") #:indirect "shell/pipeline"] library.
This library exposes a subset of its functionality in a line-based syntax.
So if you look at the docs for the pipeline library, you should see how
this just wraps it.

To get started, simply @code{(require rash)} in your program.
You can then use @code|{(rash "string of rash code")}|.
Or use @code{#lang rash}, and your whole language is as if wrapped in the
@racket[rash] macro.

Here is some quick example code:

@codeblock|{
#lang rash

;; basic pipeline stuff
ls -l /dev | grep tty | wc

;; escape to racket
ls $(if (even? (random 10)) '-a '-l)

;; use the &$() to make a line just be normal racket without
;; being wrapped into a pipeline
&$(define my-favorite-flag "-l")
ls $my-favorite-flag

;; after escaping into racket syntax, you can escape back with rash macro and friends
ls $(rash/trim "basedir $(rash/trim \"pwd\")")

;; guillemets are enabled for nestable string delimiters
ls $(rash/trim «basedir $(rash/trim «pwd»)»)

}|

An example snippet of just using rash macros in #lang racket/base
@codeblock|{
#lang racket/base

(require rash)

;; This will call ls
;; The output will go to stdout
;; The return value will be the exit status of ls
(rash "ls")

;; This will pipe the output as expected between programs
;; The final output will go to stdout
(rash "ls | grep foobar | wc")

;; This will return a the stdout output of the pipeline as a string.
;; If the last member of the pipeline exits with a nonzero status, an exception is raised.
(rash/out "ls | wc -l")

;; Newlines separate commands.
;; The output for all of them is treated normally.
;; Only the exit status of the last line is returned.
(rash #<<EOS
ls
whoami
cowsay "hello there"
uname
EOS
)

;; You can escape to Racket with $
;; The return values of the racket segments should be strings or symbols
(define my-favorite-flag '-l)
(rash "ls $my-favorite-flag $(if 'some-test '/dev \"/etc\")")

;; Racket functions can be included in a pipeline.
;; They should read/write using current-<input/output>-port, and return 0 on success
;; To use a racket function, return a closure.
(rash "$(λ () (printf \"Hello~nWorld~n\")) | grep Hello")

;; If you have a function that takes a string and returns a string, you can shellify it.
;; shellify turns current-input-port into a string, passes it in, prints the output
;; to current-output-port, and returns 0.
(rash "ls /etc | $(shellify string-upcase) | grep HOSTNAME")

;; If a function in the pipeline raises an exception, the exception is printed to current-error-port and it gives a nonzero exit status
(rash "$(λ () (error 'something \"this is an error\")))")

;; So... there you have it.
(define (my-grep pat)
  (λ () (for ([line (port->lines (current-input-port))])
          (when (regexp-match pat line)
            (displayln line)))
        0))

(rash/out #<<EOS
$(λ () (printf "hello\nmars\njupiter\nand\nneptune")) | $(shellify string-upcase) | $(my-grep "R")
EOS
)

;; As you can see, it is much better with a nestable string delimiter like «» in #lang udelim or #lang rash

}|




@section{Reference}

TODO - but first I should nail down the syntax, etc.

But variants of the rash macro are:

rash/out -- returns output as string.

rash/trim -- like rash/out, but does string-trim to it.

rash/number -- runs string->number on the output of rash/trim.

@section{RASH repl}

To launch a rash repl, run @code{racket -l rash/repl}, or if you have
your racket's @code{bin} dir on your path, you can run
@code{rash-repl}.

This repl is very rough.  It is basically awful.  At this point it is
just something I cooked up really quick because I wanted to have a
repl.  Eventually, I hope to make a nice, usable interactive shell repl.

The syntax in the repl is the same as in @code{#lang rash}.

The repl also loads ~/.config/rash/rashrc, which must be written in
the language of #lang rash, but not have #lang rash at the top.
Yeah... that should be improved.  But basically it's meant to be
evaluated interactively so it can muck up your interactive namespace
with definitions and requires that you want in an interactive shell.

Here are some big things you will feel are missing:

@itemlist[
@item{Completion (tab or otherwise)}
@item{expansion of ~, $ENVIRONMENT_VARS, /generally/$variables/in/paths, globs, etc}
@item{(line) editing (but you can wrap it with rlwrap!)}
@item{customizable prompts}
@item{syntax to pipe output to a file -- it's in the pipeline library, there's just no syntax here yet.}
@item{strings/command output as temp files to pass as arguments to commands (IE <() in bash)}
          ]

I'm not sure what I want to do about all of the expansion stuff -- especially since globs are an easy way I've shot myself in the foot many times.  But ~ expansion is so convenient, and so is path interpolation.

At any rate, fancy stuff would be great to add at some point.

But you know what's not missing?
Real, recursive data structures, a module system, macros, closures, ... everything
that Racket provides that most shell languages (and many other languages...) are
sorely missing.

@section{Code and License}

The code is available
@hyperlink["https://github.com/willghatch/racket-rash"]{on github}.

This library is licensed under the terms of the LGPL version 3, or (at
your option) any later version published by the Free Software
Foundation (IE LGPL3+).
