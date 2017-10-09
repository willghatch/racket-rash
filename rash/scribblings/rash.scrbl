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

@section{Stability}

Rash is not entirely stable.  It is not quite ready for a stable release.  But it is definitely ready to have fun with!

@section{RASH Guide}

Rash is a language and library for shell programming.  It provides a nice line-based syntax that make many shell interactions feel similar to other common shells, and provides easy program pipelining by re-providing from
@secref["pipeline-macro"
        #:doc '(lib "shell/scribblings/shell-pipeline.scrbl")].

Here is a quick example to show what it looks like:

@verbatim|{
#lang rash

;; For the moment this is required to get short names like |
(require rash/demo/setup)

;; Pretend there are some require forms here (that I'm too lazy to add
;; while writing documentation on a plane) for anything that isn't
;; in scope with just #lang rash

;; the beginning and end of lines act like parens
run-pipeline ls | grep foobar

;; Also, | is treated as a normal alphabet character in the rash reader.
;; So let's assume it's bound to a unixy pipe-operator.

;; Every line starts with a line-macro.
;; If it doesn't have one, a default is inserted.
;; This default is, by default, run-pipeline.
ls | grep foobar

;; We can make all sorts of interesting piplines that include
;; external processes and racket functions
;; See the run-pipeline docs for more information.
ls |> string-upcase | cowsay

;; Inside a pipeline you can use parentheses
;; to escape to Racket.
;; Here we compute the arguments to `ls`.
ls (list '-l (or some-dir other-dir)) | grep foobar

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
[if use-clang? 'clang 'gcc] -o foo foo.c


;; If you want to break up a long line, you can comment
;; out the newline like so.  I will add \ to escape the
;; newline like other shell languages, but I haven't yet
;; because I think the way I'll add it is gross, but
;; sooner or later I will definitely add it.
ls -la my-directory-that-has-a-long-name | grep foobar #|
|# |> string-upcase | cowsay

;; If you want to turn one line into two logical lines like
;; this bash snippet: `ls ; cd ..`. too bad.
;; But I'm considering making a single ; be a line
;; break instead of a comment (thus necessitating two ;;
;; characters to quote).  I would love to hear feedback from
;; people as to whether or not they would like that.

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

See the udelim docs for more detailed information on the string delimiters.  TODO - add link.  The extra delimiters provided by udelim are enabled by default in #lang rash and inside the @racket[rash] macro.

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


At the moment #lang rash prints the values of top-level expressions, like #lang racket.  I generally hate that.  I will probably change that, but for the moment I'll leave it.  When I finish making the forms to define new #langs I will add an option to set that to your liking for custom versions, and probably make the default not print.

Rash is also useful as an interactive repl that feels like a nice mix between a normal Racket repl and an interactive Bash shell.








@section{RASH Reference}

Also be sure to see
@secref["pipeline-macro"
        #:doc '(lib "shell/scribblings/shell-pipeline.scrbl")]
for documentation on running pipelines, defining and using pipeline operators, etc.

@defform[(rash options ... codestring)]{
Read @racket[codestring] as rash code.

Options:

@racket[#:out] sets the default output port or transformer for unix pipelines (as run by @racket[run-pipeline]).  The default runs @racket[port->string] on the output.

@racket[#:in] sets the default input port for unix pipelines (as run by @racket[run-pipeline]).  The default is an empty port.

@racket[#:err] sets the default error port for unix pipelines (as run by @racket[run-pipeline]).  The default is to turn the errors into a string that is put into the exception generated by an error in the pipeline.

@racket[#:default-starter] sets the default starting pipeline operator for @racket[run-pipeline] when one is not explicitly given in the pipeline.  The default is one of the simple unixy ones... TODO - this default will probably change.

@racket[#:default-line-macro] sets the default line-macro for lines that don't explicitly have one.  The default is the run-pipeline line-macro.

TODO - options for changing the reader, etc.

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

You can run the repl by running @code{racket -l rash/repl}.  It has no line editing currently, so it's a little nicer if you run it with the rlwrap command.

How to run the repl and various details of how it works and what is available might change in the near future.  I may, for instance, add some sort of make-repl-command macro similar to @racket[make-rash-transformer] to change defaults at a very low level.  As it is defaults are changed by having variables that you essentially @code{set!} in the repl, and there are some rc files that are loaded.

First, if $HOME/.config/rash/rashrc.rkt exists, it is required at the top level of the repl.  Then, if $HOME/.config/rash/rashrc (note the lack of .rkt) exists, it is evaluated at the top level more or less as if typed in (much like rc files for bash and friends).  There is an example rashrc file in the project repo's demo directory.

To really just get going with the repl, put the following into $HOME/.config/rash/rashrc:

@verbatim|{
(require rash/demo/demo-rc.rkt)
set-default-pipeline-starter! |
}|


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


@section{Demo file with the good stuff that I haven't yet put in the main implementation}
Many interesting things and the setup you really want for running a repl are still in a demo file.  Largely this is because a lot of the pipeline operators I've defined are basically quick hacks, and while they do interesting things, I want to write better implementations of most of them.  Especially for unix operators -- I have written several that have one feature, but I want to write one with all the features, which is harder.

To use the demo, @code{(require rash/demo/setup)}.  Also look at the file to see some examples.


@section{Code and License}

The code is available
@hyperlink["https://github.com/willghatch/racket-rash"]{on github}.

This library is licensed under the terms of the LGPL version 3, or (at
your option) any later version published by the Free Software
Foundation (IE LGPL3+).
