#lang scribble/manual

@title[#:tag "rash"]{RASH: RAcket SHell Library}
@author+email["William Hatch" "william@hatch.uno"]

@defmodule[rash]
@(require
(for-label
rash
(prefix-in shell/pipeline-macro/ shell/pipeline-macro)
))

@bold{Rash}, @italic{adj} 1.  Hurrying into action or assertion without due caution and regardless of prudence, hasty, reckless, precipitate.  “@italic{A rash programmer is likely to produce shell scripts.}”


I made a quick demo recording of an interactive repl
@hyperlink["https://asciinema.org/a/sHiBRIlSM9wHDetDhsVjrCaZi"]{here}.

Also I gave a talk at RacketCon 2017 about it, which can be viewed
@hyperlink["https://www.youtube.com/watch?v=yXcwK3XNU3Y&index=13&list=PLXr4KViVC0qIgkwFFzM-0we_aoOfAl16Y"]{here}.


@section{Stability}

Rash is not entirely stable.  It's still missing features, and there are some names and APIs I want to re-visit.  It's in active development.

However, I use it as my default shell on my laptop, and already like it much better than Bash.  I also never launch the Racket repl anymore because Rash does everything the repl does.  Try it out, and please report bugs and feedback to me!  (Note that at the time of writing, completion and line editing work but are in the readline branch waiting for the next upstream release of the readline package that it relies on.)

@section{RASH Guide}

Rash is a language and library for shell programming.  It provides a nice line-based syntax that make many shell interactions feel similar to other common shells, and provides easy program pipelining by re-providing from
@secref["pipeline-macro"
        #:doc '(lib "shell/scribblings/shell-pipeline.scrbl")].

Here is a quick example to show what it looks like:

@verbatim|{
#lang rash

;; the beginning and end of lines act like parens
run-pipeline ls | grep foobar

;; Every line starts with a line-macro.
;; If it doesn't have one, a default is inserted.
;; This default is, by default, run-pipeline.
ls | grep foobar

;; We can make all sorts of interesting piplines that include
;; external processes and racket functions
;; See the run-pipeline docs for more information.
ls |> string-upcase | cowsay

;; Note that the pipe (|) character is normal in the
;; Rash reader, unlike the default Racket reader.
;; In normal Racket, you would type \| or \|>.
;; | is actually short for =unix-pipe=.
;; |> is short for =object-pipe=.
;; By convention, pipeline operators are named with = signs.
;; Because they sort of look like pipes.  =I= =guess=.
;; I have made a few handy pipeline operators in the
;; demo directory of the shell-pipeline library.
;; At some point I'll put the most useful ones in the standard
;; exports of the library.  But you can also make your own.

;; Inside a pipeline you can use parentheses
;; to escape to Racket.
;; Here we compute the arguments to `ls`.
ls (list '-l (if (even? (random 2)) '-a '-h)) | grep foobar

;; If a line starts with a parenthesis, it is
;; treated as a normal Racket form -- no line macro
;; nonsense.
(define (add6 x) (+ 6 x))
(require racket/match)

;; This creates one potential ambiguity:
;; If you really want parens at the start of a
;; line with an implicit line macro.
;; For example, here we want to compute which
;; compiler to use:
;;
;; WRONG
;; (if use-clang? 'clang 'gcc) -o foo foo.c
;;
;; We can use another type of paren, like a bracket
;; or brace.  It's a bit of an ugly hack, but
;; the convenience of allowing parens to make
;; normal racket forms is worth it.
;;
;; RIGHT
(define use-clang? (even? (random 2)))
[if use-clang? 'clang 'gcc] -o foo foo.c
;; But this could potentially change in the future.
;; I'm considering making a line that starts with any type of
;; paren be a non-line-macro line.  So you would have to
;; explicitly add a line-macro or the starter pipe.
| (if use-clang? 'clang 'gcc) -o foo foo.c

;; Pipelines can pass objects as well as byte streams.
;; The =unix-pipe= operator returns a port, and the
;; =object-pipe= automatically converts it to a string.
;; But =unix-pipe= has a convenience to add a parser
;; with the #:as flag.
(require json)
echo "[1, 2, 3]" #:as read-json |> map add1

;; The =unix-pipe= also supports some things you might expect
;; in a Unix shell -- ~ expansion, $ENVIRONMENT_VARIABLE
;; expansion, glob expansion, and $local-variable expansion.
(define my-new-dir "my-new-dir")
cp ~/my-dir/*.rkt $HOME/$my-new-dir/

;; The =unix-pipe= also supports aliases.
(define-simple-pipeline-alias ls 'ls '--color=auto)
;; Now ls has color.
ls $XDG_CONFIG_HOME

;; If you want to break up a long line, you can comment
;; out the newline like so.  I will add \ to escape the
;; newline like other shell languages, but I haven't yet
;; because I think the way I'll add it is gross, but
;; sooner or later I will definitely add it.
ls -laH /sys/class/power_supply/BAT0 | grep now #|
|# |> string-upcase | cowsay

;; If you want to turn one line into two logical lines like
;; this bash snippet: `ls ; cd ..`. too bad.
;; I'm considering adding something to break up a line, but
;; I haven't decided how I want to do it yet.

;; When a pipeline is unsuccessful, an exception is thrown.
;; This when a command in the middle of the pipeline
;; is unsuccessful.  There are some flags for controlling
;; the specific behavior.

}|

We can use rash not only by using #lang rash, but also by using the @racket[rash] macro in any other language:

@verbatim|{
#lang racket
(require rash)
(rash "ls -l")
}|

The macro reads the string at compile time and turns it into syntax-objects with proper hygiene information, so within the rash code you can refer to variables in scope at the @racket[rash] use site.  But normal strings are inconvenient -- they require escaping to write many things, and they don't provide enough information to get exact location information.  I use string delimiters from the udelim package instead:

@verbatim|{
#lang udelim racket
(require rash)
(rash «ls -l»)

;; These weird strings are especially useful for nesting.
;; You don't have to quote anything, just balance the delimiters.
(rash «cat (rash «which myscript.rkt»)»)
}|

See the udelim docs for more detailed information on the string delimiters (particularly @racket[make-string-delim-readtable]).  The extra delimiters provided by @racket[make-udelim-readtable] are enabled by default in #lang rash and inside the @racket[rash] macro.

@verbatim|{
#lang udelim racket
(require rash)

;; The rash macro is like a `begin`.
(rash «ls -l
       cd ..
       cowsay "hello"»)

;; It splices into definition contexts like begin
(rash «(define foo 5)
       ;; assume def is a line macro that defines things
       ;; and uses do-line-macro.
       def rktfiles find . -regex rkt$
       »)
(+ foo 7)
(display rktfiles)
}|

One convenient thing to do is use other udelim string delimiters that produce #%identifiers.

@verbatim|{
#lang rash
(define-syntax #%upper-triangles (make-rename-transformer #'rash))
;; Now you can invoke the Rash macro just by using ◸◹ as parens.

;; I am always using this pattern in shells to see what I wrote in my scripts
cat ◸which myscript.rkt◹
}|

The line syntax has no special control flow forms -- when I'm ready to write control flow, I'm ready to write a parenthesis.  So just use normal Racket control flow forms, and use @racket[rash] or @racket[make-rash-transformer] to get back into rash.

@verbatim|{
#lang rash

(if want-capitals?
    (rash «ls -l |> string-upcase»)
    (rash «ls -l»))
}|

Since normal Racket code essentially always starts with parens, we can use #lang rash in place of languages that use the normal reader for a lot of things -- it's almost a superset of functionality.  The one thing that breaks when switching to #lang rash is identifiers or other non-parenthesized code at the top level of a module.  But the only real reason to do that is to print the value of a variable, which can easily be done by wrapping in parens or using a line macro.

@verbatim|{
#lang racket/base

(define (foo x)
  (* 37.25 x))
(define bar (foo 22))

;; get the value
bar
}|

Can translate to

@verbatim|{
#lang rash

(define (foo x)
  (* 37.25 x))
(define bar (foo 22))

(define-line-macro id (syntax-parser [(_ e) #'e]))
id bar
;; or you could do this
;; (values bar)
}|

#lang rash differs from #lang racket in its treatment of top-level expressions.  #lang racket prints the result of top-level expressions, but I hate that.  #lang rash does not print the value of top-level expressions, so you should explicitly print any racket forms.  If #lang rash did print results of top-level expressions, you would get pipeline results (eg. 0 when a unix pipeline is successful), and that could be annoying.

Rash is also useful as an interactive repl that feels like a nice mix between a normal Racket repl and an interactive Bash shell.


@section{RASH Reference}

Note that all the pipeline stuff (@racket[run-pipeline],
@racket[=unix-pipe=], @racket[=object-pipe=],
@racket[define-pipeline-operator], etc) are documented in the
@secref["pipeline-macro"
        #:doc '(lib "shell/scribblings/shell-pipeline.scrbl")]
module.

@defform[(rash options ... codestring)]{
Read @racket[codestring] as rash code.

Options:

@racket[#:out] sets the default output port or transformer for unix pipelines (as run by @racket[run-pipeline]).  The default runs @racket[port->string] on the output.

@racket[#:in] sets the default input port for unix pipelines (as run by @racket[run-pipeline]).  The default is an empty port.

@racket[#:err] sets the default error port for unix pipelines (as run by @racket[run-pipeline]).  The default is to turn the errors into a string that is put into the exception generated by an error in the pipeline.

@racket[#:default-starter] sets the default starting pipeline operator for @racket[run-pipeline] when one is not explicitly given in the pipeline.  The default is one of the simple unixy ones... TODO - this default will probably change.

@racket[#:default-line-macro] sets the default line-macro for lines that don't explicitly have one.  The default is the run-pipeline line-macro.

TODO - options for changing the reader, etc.

Note that the input/output/error-output have different defaults for the rash macro than for the #lang or repl.

}

@defform[(make-rash-transformer options ...)]{
This takes all the same options as @racket[rash], but doesn't take a code string.  It produces a transformer like @racket[rash], but with different default values for the available options.

@(racketblock
(define-syntax my-rash (make-rash-transformer #:default-starter #'=basic-object-pipe=)))
}

@;@defform[(make-rash-module-begin-transformer options ...)]{
@;This takes all the same options as @racket[rash], but doesn't take a code string.  It produces a transformer like #%module-begin in #lang rash, but with different defaults and reader options.
@;
@;Use it to make a #lang that is like #lang rash but customized to your liking.
@;
@;@(racketblock
@;(define-syntax my-rash-module-begin
@;  (make-rash-module-begin-transformer #:default-starter #'=basic-object-pipe=)))
@;}

TODO - finish make-rash-reader-submodule and document it -- it should be like @racket[make-rash-transformer] only in should essentially create a #lang.
Note that the default #lang rash has its input/output/error-output as stdin/stdout/stderr, which is different than the rash macro.

things to document:

rash/wired



from linea:

@defform[(define-line-macro name transformer-expr)]{

Defines a line-macro (the type of macro that is required at the start of a rash line).

@verbatim|{
#lang rash

(define-line-macro my-for
  (λ (stx)
    (syntax-parse stx
      [(_ i:id from-expr body ...+)
       #'(for ([i from-expr])
           (body ...))])))

my-for f (list "file1.txt" "file2.txt") ◸rm (id f)◹
}|

}

@defform[(do-line-macro maybe-line-macro arg ...)]{
Run the arguments as if they are a rash line.  If the first argument is a line macro the line is essentially left as-is.  Otherwise the @racket[default-line-macro] is inserted.
}

@defform[#:id default-line-macro default-line-macro]{
Syntax parameter used to determine which line macro to place when one is not explicitly given.

TODO - example setting and what is the default.
}

docs about reader

how to change the inside/outside readtable

@defform[#:kind "line-macro" (cd directory)]{
Change directory to given directory.  The directory is quoted, so just put a literal path or a string.

If no argument is given, it changes to the user's home directory.

Eventually this will be replaced by a better version, but it should be backwards compatible.
}

@defform[#:kind "line-macro" (run-pipeline arg ...)]{
Same as @racket[shell/pipeline-macro/run-pipeline], except wrapped as a line-macro.
}

@section{Interactive Use}

You can run the repl by running @code{racket -l rash/repl}.  An executable named @code{rash-repl} is installed in Racket's bin directory, so if you have it on your path you can run @code{rash-repl} instead.  It has no line editing currently, so it's a little nicer if you run it with the rlwrap command.  (readline support, including basic tab completion, is written, but depends on features in the development version of Racket's readline FFI library.)

Various details of the repl will change over time.

Note that in the repl the default input/output/error-output are to the stdin/out/err connected to the terminal unless you change them.  This is different than the rash macro, and allows you to do things like run curses programs that have access to terminal ports.

The repl can be customized with rc files.
First, if $HOME/.config/rash/rashrc.rkt exists, it is required at the top level of the repl.  Then, if $HOME/.config/rash/rashrc (note the lack of .rkt) exists, it is evaluated at the top level more or less as if typed in (much like rc files for bash and friends).

A few nice things (like stderr highlighting) are in a demo-rc file you can require.  To do so, add this to $HOME/.config/rash/rashrc:

@verbatim|{
(require rash/demo/demo-rc.rkt)
}|

(Rash actually follows the XDG basedir standard -- you can have rashrc.rkt or rashrc files in any directory of $XDG_CONFIG_HOME or $XDG_CONFIG_DIRS, and the rash repl will load all of them)

All the following repl functions are not stable.

@defproc[(result-n [n integer?]) any/c]{
Only available in the repl.  Return the result of the @racket[n]th interactive command.
}

@defproc[(return-n [n integer?]) any/c]{
Only available in the repl.  Like result-n, but if the result is a pipeline, get the return value of it.  In the repl, the default line-macro is like @racket[run-pipeline], but always prepending the @racket[&pipeline-ret] flag.  So you get pipeline objects instead of their results generally, and the prompt handles it specially to see the return value, but also to potentially use other information from the pipeline object.
}

@defform[(set-default-pipeline-starter! new-starter)]{
Only available in the repl.  A line-macro that mutates the default pipeline starter used in the repl.  It's not really hygienic, so if you defined macros that used @racket[run-pipeline] without an explicit starter, this will change the result of new calls to that macro.  Basically a hack to be able to set it since I haven't figured out a better way to do it yet, aside from maybe having people make their own repl modules that set some defaults, and I'm not sure I like that plan.
}

@defparam[current-prompt-function prompt procedure?]{
You can set this parameter to change the prompt.  The prompt is responsible for printing the result returned by whatever was last run as well as showing any information you want to see.  Right now I would just stick with the default, but I plan on making a nice library of useful functions to put in a prompt (eg. functions for displaying git information, path information with length pruning, ...), and have a way of easily connecting some prompt pieces to make something good (including a way to customize how results are displayed -- I want to, for example, have types of objects that can be recognized to print tables nicely, etc).

The given function's arity is tested to see if it receives various keywords, so that the protocol can be extended and the user only has to specify the keywords that are needed for a given prompt.

Keywords optionally given:

@racket[#:last-return-value] - fairly self explanatory.  If multiple values were returned, they will be given as a list.  This will be @racket[(void)] for the prompt before the first command.

@racket[#:last-return-index] - This increments once for every command run in the repl.  It will be 0 for the prompt before the first command.  This is the index that can be used for @racket[result-n] and @racket[return-n].
}


@section{Demo stuff}

I have some proof-of-concept pipes and things in the demo directories of the rash and shell-pipeline repositories.  Eventually some of the good stuff from them will probably be improved and moved into the main modules.  But you can check them out for ideas of some things you might do.

To use the demo, @code{(require rash/demo/setup)}.  Also look at the file to see some examples.


@section{Code and License}

The code is available
@hyperlink["https://github.com/willghatch/racket-rash"]{on github}.

This library is licensed under the terms of the LGPL version 3, or (at
your option) any later version published by the Free Software
Foundation (IE LGPL3+).
