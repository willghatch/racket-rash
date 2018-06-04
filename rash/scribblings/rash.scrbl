#lang scribble/manual

@title[#:tag "rash"]{Rash: The Reckless Racket Shell}
@author+email["William Hatch" "william@hatch.uno"]

@defmodule[rash]
@(require
(for-syntax
racket/base
syntax/parse
)
(for-label
rash
(only-in rash/demo/setup in-dir =map= =filter= =foldl=)
(except-in racket/base _ do)
racket/port
syntax/parse
(prefix-in shell/pipeline-macro/ shell/pipeline-macro)
))

@(define-syntax (irash stx)
   (syntax-parse stx
     [(_ e1 e ...)
      #`(codeblock #:keep-lang-line? #f
                   #,(datum->syntax #'e1 "#lang rash")
                   "\n" e1 e ...)]))

@bold{Rash}, @italic{adj} 1.  Hurrying into action or assertion without due caution and regardless of prudence, hasty, reckless, precipitate.  “@italic{A rash programmer is likely to produce shell scripts.}”

@section{Stability}

Rash is not stable.

But it's getting closer, and it's definitely usable as an interactive shell/repl.

@section{Rash Guide}

Rash is a shell language embedded in Racket.  It has a concrete syntax that is amenable to quick and easy interactions without lots of punctuation overhead.  It aims to allow shell-style interaction and programming to be freely mixed with more general-purpose Racket code.  Like shells you can use it as a day-to-day interface for computing, run programs, wire processes together, etc.  You can copy interactive code into a file and have a working script.  Then you can generalize from there.  However, you have access to all of Racket, its data structures, its libraries, its macro system, and its other DSLs as you make your scripts more general.  Also it allows shell-style process wrangling to be mixed with Racket code, pipelines of functions that pass objects, and much more.  You can gradually move between shell-style Rash code and more normal and general Racket code in different parts of a script, or throw verbatim interactions directly into existing programs as you explore the solution space.

@; TODO - section links for shell/pipeline-macro and linea
@;Rash is essentially the combination of two parts:  the shell/pipeline-macro library, which provides a DSL for pipelining processes and Racket functions, and the Linea reader, which provides a line-oriented alternative way to write s-expressions.

Here follows a quick overview that assumes some knowledge of shell languages like Bash as well as Racket.

Rash can do the sorts of things you expect from shell languages, and a lot of cues for syntax have been taken from traditional Bourne-derived shells.  The following program works as you would expect.@margin-note{Note that Rash is not remotely Posix Shell compliant.}

@codeblock{
#lang rash
cd project-directory
echo How many Racket files do we have?
ls *.rkt | wc -l
}

You can use Racket functions in pipelines.

@irash{
;; This returns the hostname as a string in all caps
cat /etc/hostname |> port->string |> string-upcase
}

Pipelines pass objects.  When a process pipeline segment is next to a function pipeline segment, it passes a port to the function segment.  When a function segment is followed by a process segment, the return value of the function segment is printed to the stdin of the process.

The |>> operator is like the |> operator except that if it receives a port it converts it to a string automatically.  So the above example can be simplified as:

@irash{
cat /etc/hostname |>> string-upcase
}

You can also make pipelines composed entirely of Racket functions.

@irash{
|> directory-list |> map delete-file
}

Pipelines always start with an operator, and if none is specified the @racket[default-pipeline-starter] is inserted.  Pipeline operators are user-definable with @racket[define-pipeline-operator].  Defining new operators can help make common patterns shorter, simpler, and flatter.  For instance the @racket[=map=] operator wraps the @racket[map] function, allowing you to specify just the body of a lambda.

@irash{
;; These two are the same.
|> list 1 2 3 |> map (λ (x) + 2 x)
|> list 1 2 3 =map= + 2
}

Pipeline operators are macros, and therefore can play any of the tricks that macros generally can in Racket.  The | operator can auto-quote symbols, turn asterisks into glob expansion code, etc.  The |> operator can detect whether the @racket[current-pipeline-argument] is used and insert it automatically.

If you put parentheses in the middle of a pipeline, you escape to normal Racket code.

@irash{
;; This will either show hidden files or give a long listing
ls (if (even? (random 2)) '-l '-a)
}

Lines of code in Rash are command pipelines by default, but there are key words called line-macros that can change the behavior arbitrarily.
@margin-note{Line-macros can be used to make C-like control-flow forms like for, try/catch, etc, to make one-off non-pipeline forms like @racket[cd], or even to make entirely new and different line-oriented languages.}

@irash{
in-dir $HOME/project {
  make clean
}
}

For instance, @racket[in-dir] executes code with @racket[current-directory] parameterized based on its first argument.  Note that logical rash lines don't necessarily line-up with physical lines.  Newlines can be escaped with a backslash, commented out with multiline comments, and if they are inside parentheses or braces they are handled by the recursive read.

@irash{
echo This is \
     all #|
     |# one (string-append
             "logical"
             "line")
}

Braces trigger a recursive line-mode read.  They are available in line-mode as well as in the embedded s-expression mode.  So they can be used to create blocks in line-mode as with the above @racket[in-dir] example, or be used to escape from the s-expression world to line-mode.

Note that subprocess pipelines are connected to stdout and stdin in the REPL and at the top level of @tt{#lang rash}.  The most common thing you want when embedding some Rash code is for subprocess output to be converted to a string.  Using @code{#{}} switches to line-mode with defaults changed so that subprocess output is converted to a string and passed through @racket[string-trim].

@irash{
;; I do this a lot when I don't remember what a script does
cat #{which my-script.rkt}
}

TODO - how to more generally parameterize such settings.

Every line in Rash is actually a line-macro.  If a line does not start with a line-macro name explicitly, @racket[default-line-macro] is inserted.  By default this is @racket[run-pipeline] in Rash.

TODO - actually the default is run-pipeline/logic, which I haven't documented yet, which adds && and ||.

You can also write normal parenthesized Racket code.  If the first (non-whitespace) character on a line is an open parenthesis, the line is read as normal Racket code and no line-macro is inserted.

@irash{
(define flag '-l)
ls $flag
}

Note that practically all Racket code starts with an open-paren, so Rash is almost a superset of normal Racket.  The only thing lost is top-level non-parenthesized code, which is really only useful to see the value of a variable.  Most programs in @tt{#lang racket/base} could be switched to @tt{#lang rash} and still function identically, and I never launch the Racket repl anymore because rash-repl is both a shell and a full Racket repl.

@irash{
(define x 1234)
;; Now let's see the value of x.
;; We can't just write `x`, but we can do any of these:
(values x)
|> values x
echo $x
val x
}

Avoiding line-macros by starting with a paren causes an annoying inconsistency -- you can't have a line-macro auto-inserted if the first argument to the line macro is a parenthesized form.

@irash{
;; We want to choose a compiler at runtime.
run-pipeline (if use-clang? 'clang 'gcc) -o prog prog.c
;; If we leave off the line-macro name, it will not be inserted
;; because the line starts with a parenthesis
(if use-clang? 'clang 'gcc) -o prog prog.c
}

This problem can be fixed by prepending the line-macro name or by using brackets instead of parentheses.  The issue doesn't come up much in practice, and it's a small sacrifice for the convenience of having both normal Racket s-expressions and Rash lines.

What else belongs in a quick overview?  Pipelines with failure codes don't fail silently -- they raise exceptions.  More fine-grained behavior can be configured.  There are probably more things I should say.  But you can read the references for Rash, @racket[run-pipeline], and Linea.  (Rash is really just a combination of Linea and the Shell Pipeline library.)  I will replace this with better documentation soon, but I wrote this up quickly to replace the even worse and terribly out-of-date documentation that was here before.

Linea documentation:
@secref["linea"
        #:doc '(lib "linea/scribblings/linea.scrbl")]

Pipeline macro documentation:
@secref["pipeline-macro"
        #:doc '(lib "shell/scribblings/shell-pipeline.scrbl")]

Also, for those who just want to pipeline subprocesses in a Racket program using a lispy syntax, you probably want the shell/pipeline library, a basic component of Rash that can be used on its own:
@secref["pipeline"
        #:doc '(lib "shell/scribblings/shell-pipeline.scrbl")]



@;@section{Media}
@;I made a quick demo recording of an interactive repl
@;@hyperlink["https://asciinema.org/a/sHiBRIlSM9wHDetDhsVjrCaZi"]{here}.
@;It's a little out of date.  I should make a new and better one.
@;
@;Also I gave a talk at RacketCon 2017 about it, which can be viewed
@;@hyperlink["https://www.youtube.com/watch?v=yXcwK3XNU3Y&index=13&list=PLXr4KViVC0qIgkwFFzM-0we_aoOfAl16Y"]{here}.
@;There have been some changes since the talk was given, but the core ideas are the same.



@section{Rash Reference}

Note that all the pipeline stuff (@racket[run-pipeline],
@racket[=unix-pipe=], @racket[=object-pipe=],
@racket[define-pipeline-operator], etc) are documented in the
@secref["pipeline-macro"
        #:doc '(lib "shell/scribblings/shell-pipeline.scrbl")]
module.

TODO - rash configuration forms, reader modification...

@defform[(rash options ... codestring)]{
Deprecated.

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
Deprecated.

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

@;TODO - finish make-rash-reader-submodule and document it -- it should be like @racket[make-rash-transformer] only in should essentially create a #lang.
Note that the default #lang rash has its input/output/error-output as stdin/stdout/stderr, which is different than the rash macro.



@; TODO - this is from linea, probably move it:

@defform[(define-line-macro name transformer-expr)]{

Defines a line-macro (the type of macro that overrides the behavior of a rash line).

@codeblock|{
#lang rash
(require (for-syntax racket/base syntax/parse))
(define-line-macro my-for
  (syntax-parser
    [(_ i:id (~datum in) from:id ... (~datum do) body:expr)
     #'(for ([i (list 'from ...)])
          body)]))

my-for f in file1.txt file2.txt do {
  rm $f
}
}|

}

@defform[#:id default-line-macro default-line-macro]{
Syntax parameter used to determine which line macro to place when one is not explicitly given.

TODO - example setting and what is the default.
}

@; TODO - docs about reader

@; TODO - how to change the inside/outside readtable

@defform[#:kind "line-macro" (cd directory)]{
Change directory to given directory.  The directory is quoted, so just put a literal path or a string.

If no argument is given, it changes to the user's home directory.

Eventually this will be replaced by a better version, but it should be backwards compatible.
}

@defform[#:kind "line-macro" (run-pipeline arg ...)]{
Same as @racket[shell/pipeline-macro/run-pipeline], except wrapped as a line-macro.
}

@section{Interactive Use}

You can run the repl by running @code{racket -l rash/repl}.  An executable named @code{rash-repl} is installed in Racket's bin directory, so if you have it on your path you can run @code{rash-repl} instead.

Various details of the repl will change over time.

Note that in the repl the default input/output/error-output are to the stdin/out/err connected to the terminal unless you change them.  This is different than the rash macro, and allows you to do things like run curses programs that have access to terminal ports.

The repl can be customized with rc files.
First, if $HOME/.config/rash/rashrc.rkt exists, it is required at the top level of the repl.  Then, if $HOME/.config/rash/rashrc (note the lack of .rkt) exists, it is evaluated at the top level more or less as if typed in (much like rc files for bash and friends).

A few nice things (like stderr highlighting) are in a demo-rc file you can require.  To do so, add this to $HOME/.config/rash/rashrc:

@verbatim|{
(require rash/demo/demo-rc)
}|

(Rash actually follows the XDG basedir standard -- you can have rashrc.rkt or rashrc files in any directory of $XDG_CONFIG_HOME or $XDG_CONFIG_DIRS, and the rash repl will load all of them)

The repl uses the readline module for line-editing and completion.  The readline module by default uses libedit (or something like that) instead of the actual libreadline for licensing reasons.  Libedit doesn't seem to handle unicode properly.  Installing the readline-gpl package fixes that (@tt{raco pkg install readline-gpl}).

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


@;@section{Demo stuff}
@;
@;I have some proof-of-concept pipes and things in the demo directories of the rash and shell-pipeline repositories.  Eventually some of the good stuff from them will probably be improved and moved into the main modules.  But you can check them out for ideas of some things you might do.
@;
@;To use the demo, @code{(require rash/demo/setup)}.  Also look at the file to see some examples.
@;

@section{Demo stuff reference}

I've written various pipeline operators and line macros that I use, but I haven't decided what should be in the default language yet.  So for now they are sitting in a demo directory.  But I need some examples.  So here I'm documenting a bit.

Use it with @tt{(require rash/demo/setup)}.
@(declare-exporting rash/demo/setup)


@defform[#:kind "pipeline-operator" (=map= arg ...)]{
Sugar to flatten mapping.

Eg.

@irash{
|> list 1 2 3 4 =map= + _ 1
}
is equivalent to
@irash{
(map (λ (x) (+ x 1)) (list 1 2 3 4))
}

The @racket[_] argument is appended to @racket[=map=]'s argument list if it is not written explicitly.
}

@defform[#:kind "pipeline-operator" (=filter= arg ...)]{
Sugar to flatten filtering.

Eg.

@irash{
|> list 1 2 3 4 =filter= even?
}
is equivalent to
@irash{
(filter (λ (x) (even? x)) (list 1 2 3 4))
}

The @racket[_] argument is appended to @racket[=filter=]'s argument list if it is not written explicitly.
}


@defform[#:kind "line-macro" (in-dir directory body)]{
Dollar and glob expands the @racket[directory], then executes the @racket[body] with @racket[current-directory] parameterized to the result of expanding @racket[directory].  If glob expansion returns multiple results, the body is executed once for each of them.

Eg.

@irash{
in-dir $HOME/projects/* {
  make clean
  make
}
}
}

@defform[#:kind "line-macro" (val expression)]{
Simply returns the @racket[expression].  This is just to work around not having access to top-level unparenthesized variables.
}



@section{Code and License}

The code is available
@hyperlink["https://github.com/willghatch/racket-rash"]{on github}.

This library is licensed under the terms of the LGPL version 3, or (at
your option) any later version published by the Free Software
Foundation (IE LGPL3+).
