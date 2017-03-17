#lang scribble/manual

@title[#:tag "rash"]{RASH: RAcket SHell Library}
@author+email["William Hatch" "william@hatch.uno"]

@defmodule[rash]
@(require rash)

@section{RASH Guide}

THIS IS ALL ALPHA AND UNSTABLE

Rash is a language and library for writing shell scripts (and including them in
larger Racket programs if desired).  It is basically a syntax wrapper for the
@other-doc['(lib "shell/shell-pipeline.scrbl") #:indirect "shell/pipeline"] library.

You can either use @code{#lang rash} or @code{(require rash)} and use
@code{(rash "string of rash code")} to embed rash syntax into any
expression position of a module in another #lang.

The following two modules are basically equivalent, except that #lang
rash does not print the values of top-level expressions:

@codeblock|{
#lang racket/base
(require rash)
(rash #<<ENDOFFILE
<program text here>
ENDOFFILE
)
}|

and

@codeblock|{
#lang rash
<program text here>
}|

Note that I don't really recommend including rash code in industrial-strength applications, and I would like to warn that shell scripts are often very brittle -- if you distribute a shell script you have to go to great lengths to ensure that the programs used by your script are installed on machines where the script is used, and even the most basic programs that you assume are available may have incompatible options (eg. GNU vs BSD options for standard Unix programs, or stripped-down busybox versions, etc).

However, shell scripting is a @emph{very} convenient way to do certain kinds of programming.  It's great for automating your workflow and doing quick prototype work.  It's great for glue code around programs that already do 90% of what you need.  And interactive shell language repls are unparalleled for live system administration tasks (which can almost just be recorded to generate an automation script).

I have loads of shell scripts that I use on my computers, from simple wrapper scripts to complex system administration tasks.  But as scripts like these want to grow and become more complex, bash and friends don't scale up to deal with the added complexity, and all kinds of frustration and crazy, dirty hack work ensues until the programmer eventually starts from scratch in a more general-purpose language.  But then you lose the domain-specific ease of wiring programs together in a simple syntax.  Rash aims to solve those problems by offloading any part of a shell script that simple program piping doesn't solve well into full-on Racket, and allowing any Racket program to slide easily into the domain of external program wiring by embedding rash macros.


Here is some quick example code:

@codeblock|{
#lang rash

;; basic pipeline stuff
ls -l /dev | grep tty | wc

;; escape to racket
ls $(if (even? (random 10)) '-a '-l)

;; if the first character on the line is an open paren, it is read as
;; normal racket until the matching close paren and not wrapped in a pipeline
(require racket/list)
(begin 'foo 'bar)
(define my-favorite-flag "-l")
ls $my-favorite-flag

;; after escaping into racket syntax, you can escape back with rash macro and friends
ls $(rash/trim "dirname $(rash/trim \"pwd\")")

;; guillemets are enabled for nestable string delimiters
ls $(rash/trim «dirname $(rash/trim «pwd»)»)

;; $$ escapes to racket but splices the result
(require file/glob)
cat $$(glob "*.rkt")

;; update timestamps on all racket files not modified in the last minute
;; (to demo that with rash you can do the sorts of things that are possible
;; with zsh extended glob qualifiers)
(define (not-modified-this-minute f)
  (< 60 (- (current-seconds) (file-or-directory-modify-seconds f))))
touch $$(filter not-modified-this-minute (glob "*.rkt"))

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
;; (IE it is like a begin)
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

;; You can also have a line be just racket if it starts with an open paren,
;; UNLESS it is the first line of a rash block.
;; This shouldn't be a huge deal, since there is no reason to use a rash macro
;; just to escape right back to normal racket...  And in #lang rash you should
;; have a newline after the #lang line at least.
;(rash "(this is bad)") ;; doesn't work right!
(rash "
(+ 5 7)")

;; Racket functions can be included in a pipeline.
;; They should read/write using current-<input/output>-port, and return 0 on success
;; To use a racket function, return a closure.
(rash "$(λ () (printf \"Hello~nWorld~n\")) | grep Hello")

;; As you can see with all the string escaping, alternating languages is much
;; nicer with a nestable string delimiter like «» in #lang udelim metalanguage
;; or #lang rash than with normal "" string delimiters.

;; If you have a function that takes a string and returns a string, you can shellify it.
;; shellify turns current-input-port into a string, passes it in, prints the output
;; to current-output-port, and returns 0.
(rash "ls /etc | $(shellify string-upcase) | grep HOSTNAME")

;; If a function in the pipeline raises an exception, the exception is printed to current-error-port and the
;; pipeline may be unsuccessful (depending on the functions position in the pipeline and #:status-and?)
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


}|




@section{Reference}

Remember, this is not yet stable!


The rash module also exports everything provided by @other-doc['(lib "shell/shell-pipeline.scrbl")].

@defform[(rash code-string)]{
Macro to use rash syntax in any expression position of another language.  Returns the result of the last expression (IE the lines are wrapped with @racket[begin]).

@codeblock|{
#lang racket/base
(require rash)

;; This is a bad example.
(if (pipeline-success? (rash "ls /etc | grep -E «^hostname$»"))
    (displayln "There is an /etc/hostname file.")
    (displayln "There is apparently not a hostname file, or ls or grep weren't found."))
}|
}

@defform[(rash/out code-string)]{
Like @racket[rash], but returns a string printed by any contained code to the @racket[current-output-port] (or stdout for any processes).

@codeblock|{
#lang racket/base
(require rash)

(define hostname-string
  ;; This will return the string "hostname\n", or raise an exception if there is an error.
  (rash/out "ls /etc | grep -E «^hostname$»"))
}|
}

@defform[(rash/trim code-string)]{
Like @racket[rash/out], but wraps the output with @racket[string-trim].

@codeblock|{
#lang rash
;; let's read the source of the xdg-open script (if it's found on the path).
;; Note that we need to trim the output, or the trailing newline will cause
;; `cat` to try to open a file whose name ends in a newline (which probably
;; is not what we want).
cat $(rash/trim «which xdg-open»)
}|
}

@defform[(rash/number code-string)]{
Wraps the output of @racket[rash/trim] with @racket[string->number].

@codeblock|{
#lang racket/base
(require rash)

;; Use (current-seconds), not this.  But the answer should be the same.
(define seconds-since-epoch (rash/number «date +%s»))
(define my-file "/home/me/some-file.txt")
;; Let's find out how many lines some-file.txt has.
(define file-lines (rash/number "wc -l $my-file | cut -f 1 -d « »"))
}|
}

@defproc[(rash-splice [v any/c]) rash-splice?]{
Puts the given value in a wrapper that tells rash to splice it into a @racket[pipeline-member-spec] command/argument list.  Generally in rash syntax you would use $$ instead.

Examples:
@codeblock|{
#lang rash
(require file/glob)
;; these two lines are equivalent
cat $(rash-splice (glob "*.rkt"))
cat $$(glob "*.rkt")
}|
}

@defproc[(rash-splice? [v any/c]) boolean?]{
Predicate for rash-splice objects.
}

@defform[(define-alias alias-name lambda-list body ...+)]{
Defines an alias function that can be accessed in rash syntax without needing $ escapes.

@racket[lambda-list] and @racket[body] are used to make a lambda wrapped with @racket[alias-func].  The form is also set up so that rash recognizes @racket[alias-name] and doesn't quote it when in command position in a pipeline.

Examples:
@codeblock|{
;; The main purpose of shell aliases is to make common uses really short.
(define-alias l args (list* 'ls '--color=auto args))
;; Can't remember how your common flags for find work?
(define-alias find-files args `(find ,@args -type f))
}|
}

@section{RASH repl}

To launch a rash repl, run @code{racket -l rash/repl}, or if you have
your racket's @code{bin} dir on your path, you can run
@code{rash-repl}.

This repl is very rough.  It is basically awful.  At this point it is
just something I cooked up really quick because I wanted to have a
repl.  Eventually, I hope to make a nice, usable interactive shell repl.

The syntax in the repl is the same as in @code{#lang rash}.

The repl also loads configuration.  First it top-level evaluates
@code{(require (file PATH))}, for each file
@code{$XDG_CONFIG_DIRS/rash/rashrc.rkt} (generally
~/.config/rash/rashrc.rkt), if any, where rashrc.rkt is just any
racket module (that likely requires and provides identifiers you want
to use, and generally sets up your dynamic environment).  Then for any
files @code{$XDG_CONFIG_DIRS/rash/rashrc}, it reads and evaluates them
as top-level rash code.

Here are some big things you will feel are missing (primarily in the repl):

@itemlist[
@item{Completion}
@item{expansion of ~ as $HOME, /generally/$variables/in/paths, etc}
@item{glob syntax more convenient than $$(glob "*.rkt")}
@item{(line) editing (but you can wrap it with rlwrap!)}
@item{strings/command output as temp files to pass as arguments to commands (IE <() in bash)}
          ]

But you know what's already not missing?

Recursive data structures, first class functions/closures, lexically-scoped definitions by default, a module system, hygienic macros, seamless integration with other modules and #langs, ... everything that Racket provides that most shell languages (and many other languages...) are sorely missing.

@section{Code and License}

The code is available
@hyperlink["https://github.com/willghatch/racket-rash"]{on github}.

This library is licensed under the terms of the LGPL version 3, or (at
your option) any later version published by the Free Software
Foundation (IE LGPL3+).
