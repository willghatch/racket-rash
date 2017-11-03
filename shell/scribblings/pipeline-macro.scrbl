#lang scribble/manual

@(require
(for-label shell/pipeline-macro
           shell/pipeline-macro-short-names
           (prefix-in shell/mixed-pipeline/ shell/mixed-pipeline)
           ))

@; TODO - naming...
@title[#:tag "pipeline-macro"]{Pipeline Macro Library}

@defmodule[shell/pipeline-macro]

@section{shell/pipeline-macro stability}

This library is not entirely stable.

The base set of pipeline operators is likely to change, and some of the names I want to review before a stable release.

@section{shell/pipeline-macro guide}

This module is a macro DSL wrapper around the shell/mixed-pipeline library.  It is designed for running pipelines of external processes (which pass each other ports) and Racket functions (which pass each other objects).  It does this with a very flat syntax and user-definable pipeline operators, which provide a lot of convenient sugar for making pipelines shorter.  It is particularly tailored for use in a line-based syntax, like that of the Rash language.

Here are some quick examples:

@verbatim{
;; Pipe the output of ls to grep.
(run-pipeline =unix-pipe= ls -l =unix-pipe= grep foo)

;; To save on space, let's assume % is bound to =unix-pipe=
(run-pipeline % ls -l % grep foo)
}

We can also pipeline objects.  Object pipelines are full of functions instead of process specifications.

@verbatim{
;; This will return 2
(run-pipeline =object-pipe= list 1 2 3 =object-pipe= second)

;; To save on space, let's assume %> is bound to =object-pipe=
(run-pipeline %> list 1 2 3 %> second)
}

We can mix the two:

@verbatim{
;; Capitalized ls output.  =object-pipe= automatically converts ports to strings.
(run-pipeline % ls -l %> string-upcase)
}

I am really running out of steam for documenting right now... TODO - write a good guide.

@section{shell/pipeline-macro reference}

@subsection{Running Pipelines}

@defform[#:literals (run-pipeline)
         (run-pipeline pipeline-flag ...
                       pipeline-member-spec ...
                       pipeline-flag ...)
         #:grammar [(pipeline-member-spec
                     (code:line pipe-operator pipe-operator-arg ...))
                    (pipeline-flag
                     (code:line &bg)
                     (code:line &pipeline-ret)
                     (code:line &in file-expression)
                     (code:line &< file-name)
                     (code:line &out file-expression)
                     (code:line &> file-name)
                     (code:line &>> file-name)
                     (code:line &>! file-name)
                     (code:line &err file-expression)
                     (code:line &strict)
                     (code:line &permissive)
                     (code:line &lazy)
                     (code:line &lazy-timeout timeout-expression)
                     (code:line &env env-expression)
                     )]]{
Run a pipeline.  This is a macro wrapper for @racket[shell/mixed-pipeline/run-pipeline] that uses pipeline operator macros to specify the pipeline to be run.  So you should read the docs about that as well.

The pipeline flags affect the options passed to @racket[shell/mixed-pipeline/run-pipeline] and are documented separately.

The pipeline-member-specs are transformed according to the pipeline operators given.  If the first non-flag argument to @racket[run-pipeline] is not a pipeline operator, then a default is put in its place as determined by @racket[default-pipeline-operator].  The full names of pipeline operators are conventionally identifiers surrounded with = signs.

At the time of writing I'm not really sure what to write here, so have an example:

@(racketblock (run-pipeline =object-pipe= list 1 2 3
                            =for/list= + 1 current-pipeline-argument
                            =for/list= + 1))

This returns @(racketblock (list 3 4 5)).  Notice that current-pipeline-argument is placed automatically for the second =for/list= operator -- it could have been left off of the other one as well, but I wanted to show what identifier is sneakily added.

If we instead run
@(racketblock (run-pipeline =object-pipe= list 1 2 3
                            =for/list= + 1 current-pipeline-argument
                            =for/list= + 1
                            &bg))
we will get a pipeline object back.  Conceptually it is still running when it is returned, though in this case it's likely finished by the time we can inspect it.  We can use @racket[pipeline?], @racket[pipeline-success?], @racket[pipeline-return], etc on it.

}



@subsection{Pipeline Flags}
@deftogether[(
@defform[#:kind "pipeline-flag" (&bg)]
@defform[#:kind "pipeline-flag" (&pipeline-ret)]
@defform[#:kind "pipeline-flag" (&in port-expression)]
@defform[#:kind "pipeline-flag" (&< file-name)]
@defform[#:kind "pipeline-flag" (&out port/reader-expression)]
@defform[#:kind "pipeline-flag" (&> file-name)]
@defform[#:kind "pipeline-flag" (&>! file-name)]
@defform[#:kind "pipeline-flag" (&>> file-name)]
@defform[#:kind "pipeline-flag" (&err port-expression)]
@defform[#:kind "pipeline-flag" (&strict)]
@defform[#:kind "pipeline-flag" (&permissive)]
@defform[#:kind "pipeline-flag" (&lazy)]
@defform[#:kind "pipeline-flag" (&lazy-timeout)]
)]{
These identifiers are all errors if used outside of @racket[run-pipeline].  They are essentially used in place of #:keywords to not conflict with pipeline operators that take keywords.

@racket[&<], @racket[&>], @racket[&>>], and @racket[&>!] each take a file name and cause (respectively) input redirection from the given file, output redirection to the given file erroring if the file exists, output redirection appending to the given file, and output redirection truncating the given file.  @racket[&in], @racket[&out], and @racket[&err] take an argument suitable to be passed to #:in, #:out, and #:err of @racket[shell/mixed-pipeline/run-pipeline].

@racket[&bg] and @racket[&pipeline-ret] toggle #:bg and #:return-pipeline-object, and @racket[&strict], @racket[&permissive], and @racket[&lazy] set the #:strictness argument.
}

@subsection{Pipeline Operators}

The core module only provides a few simple pipeline operators.  There are many more in the demo/ directory in the source repository.  Most of them are hastily written experiments, but some good ones should eventually be standardized.

@defform[#:kind "pipeline-operator" (=composite-pipe= (pipe-op arg ...) ...+)]{
Produces a composite pipeline member spec made from the pipeline operators given.  This is really more for use when defining new pipeline operotars than for use in a pipeline itself.
}

@defform[#:kind "pipeline-operator" (=basic-unix-pipe= options ... args ...+)]{
Produces a unix-pipeline-member-spec with the given arguments as the process/function argument list.  The arguments are not quoted.  Any argument that produces a list will be spliced into the argument list.

Options all take an argument, must precede any arguments, and are as follows:

@racket[#:as] - This is sugar for adding on an object pipeline member afterward that parses the output somehow.  This should be given either #f (no transformation), a port reading function (eg. @racket[port->string]), or one of a pre-set list of symbols:  @racket['string], @racket['trim], @racket['lines], or @racket['words].

@racket[#:e>] - Accepts a file name (as an identifier), redirects the error stream to that file.  Produces an error if the file exists.

@racket[#:e>!] - Accepts a file name (as an identifier), redirects the error stream to that file.  Truncates the file if it exists.

@racket[#:e>>] - Accepts a file name (as an identifier), redirects the error stream to that file.  Appends to the file if it exists.

@racket[#:err] - Takes an expression to produce an error redirection value suitable for @racket[unix-pipeline-member-spec].

@racket[#:success] - Takes an expression suitable for the @racket[#:success] argument of @racket[unix-pipeline-member-spec].

TODO - env modification

}

@defform[#:kind "pipeline-operator" (=quoting-basic-unix-pipe= options ... args ...+)]{
Like @racket[=basic-unix-pipe=], except that it quotes all of its arguments that are identifiers.  All non-identifier arguments (notably parenthesized forms) are not quoted, and thus you can unquote by using parentheses.

@verbatim|{
(define x "/etc")
(define-syntax id (syntax-parser [(_ x) #'x]))

;; I find I really don't mind this as a means of unquoting here.
(run-pipeline =quoting-basic-unix-pipe= ls (id x))
}|
}

@defform[#:kind "pipeline-operator" (=default-unix-pipe= args ...+)]{
This is the pipe that does more or less what you expect.  It does tilde expansion (~ -> $HOME).  It does globbing.  When you have $identifiers-with-dollar-signs they are expanded into variable references.  When $DOLLAR_IDENTIFIERS_ARE_CAPITALIZED they are expanded to environment variable lookups.

After all that expansion, it passes through to @racket[=quoting-basic-unix-pipe=].

However, if the first argument is a pipeline alias defined with @racket[define-pipeline-alias] or @racket[define-simple-pipeline-alias], then the operator from that alias is swapped in instead, skipping everything else that this operator would normally do.

@verbatim|{
(run-pipeline =default-unix-pipe= echo $HOME/*.rkt)
(define-simple-pipeline-alias d 'ls '--color=auto)
(define dfdir 'dotfiles)
(run-pipeline =default-unix-pipe= d $HOME/$dfdir)
}|

Also available in shell/pipeline-macro-short-names as @racket[\|]
}

@defform[#:kind "pipeline-operator" (=basic-object-pipe/expression= e)]{
The simplest object pipe.  @racket[e] is simply the body of a @racket[lambda].  When used as a pipeline starter, the lambda accepts no arguments.  Otherwise it is a single-argument function, and @racket[current-pipeline-argument] is used to refer to its argument.
}

@defform[#:kind "pipeline-operator" (=basic-object-pipe/form= arg ...+)]{
Creates an object pipe where @code{(arg ...)} is the body of a function.

As with other object pipes, when used as a pipeline starter it generates a lambda with no arguments, and as a pipeline joint it generates a lambda with one argument, @racket[current-pipeline-argument].
}

@defform[#:kind "pipeline-operator" (=basic-object-pipe= arg ...+)]{
Like @racket[=basic-object-pipe/form=], except that when not used as a pipeline starter, if the @racket[current-pipeline-argument] is not used within the arguments, it is appended as the last argument.

To discover whether @racket[current-pipeline-argument] is used, each argument is local-expanded.  So @code{(arg ...)} must be equivalent to a function application form and not a macro invocation form.
}

@defform[#:kind "pipeline-operator" (=object-pipe/expression= arg ...+)]{
Like @racket[=basic-object-pipe/expression=], but when it receives a port as an argument, it converts it to a string.
}
@defform[#:kind "pipeline-operator" (=object-pipe/form= arg ...+)]{
Like @racket[=basic-object-pipe/form=], but when it receives a port as an argument, it converts it to a string.
}
@defform[#:kind "pipeline-operator" (=object-pipe= arg ...+)]{
Like @racket[=basic-object-pipe=], but when it receives a port as an argument, it converts it to a string.

Also available in shell/pipeline-macro-short-names as @racket[\|>].
}

@defidform[#:kind "pipeline-operator"
         default-pipeline-starter]{
Syntax parameter determining which pipeline operator is inserted when a @racket[run-pipeline] form doesn't explicitly start with one.
}

I've written various other pipeline operators that are a little more exciting and that are currently in the demo directory of the repository.  I'll eventually polish them up and put them somewhere stable.  They include things like unix pipes that automatically glob things, unix pipes that have lexically scoped alias resolution, =filter=, =for/list=, =for/stream=,=for/list/unix-arg=,=for/list/unix-input=...

@subsection{Defining Pipeline Operators}

@defform[(define-pipeline-operator name start-or-joint ...)
#:grammar
[(start-or-joint
(code:line #:start transformer)
(code:line #:joint transformer))
]]{
Define a pipeline operator.  Pipeline operators can act differently when they are in the starting position of a pipeline or later (joint position).  Specifically, when an operator creates an @racket[object-pipeline-member-spec], it needs to have a function that accepts 0 arguments when in the start position and 1 argument in others.

If a transformer function is not specified for one of the options, a default implementation (that generates an error) is used.

The transformer will receive a syntax object corresponding to @code{(name-of-pipe argument ...)}, so it will likely want to ignore its first argument like most macros do.  But simetimes it may be useful to recur.

Example uses are in the demo directory in the repository.
}


@defform[(pipeop name syntax-parser-clauses ...+)]{
The name of this will probably change.  And maybe it will go away entirely.  I'm not sure yet.

@racket[pipeop] is a more streamlined version of @racket[define-pipeline-operator].  It defines a pipeline operator where both @racket[#:start] and @racket[#:joint] are the same.  The syntax transformer used is basically @code{(syntax-parser syntax-parser-clauses ...)}.  I made this because I thought it would be a convenient way to be able to swiftly define a new pipeline even interactively in the Rash repl.

Example uses are in the demo directory.
}


@defform[(define-pipeline-alias name transformer)]{
Defines an alias macro recognized by @racket[=default-unix-pipe=] and maybe others.

@racket[transformer] must be a syntax transformer function, and must return a syntax object that starts with a pipeline operator.

@verbatim|{
;; Unix `find` has to take the directory first, but we want
;; to always add the -type f flag at the end.
(define-pipeline-alias find-f
  (syntax-parser
    [(_ arg ...) #'(=default-unix-pipe= find arg ... -type f)]))

;; these are equivalent
(run-pipeline =default-unix-pipe= find-f .)
(run-pipeline =default-unix-pipe= find . -type f)
}|
}

@defform[(define-simple-pipeline-alias name cmd arg ...)]{
Simple sugar for @racket[define-pipeline-alias].  It defines an alias transformer that uses @racket[=default-unix-pipe=].

@verbatim|{
(define-simple-pipeline-alias ls 'ls '--color=auto)
;; these are equivalent
(run-pipeline =default-unix-pipe= d -l $HOME)
(run-pipeline =default-unix-pipe= 'ls '--color=auto -l $HOME)
}|
}

@defform[#:id current-pipeline-argument current-pipeline-argument]{
The name of the implicit argument for object pipes.  The default is an error, and pipe operators that accept it must set it up using @racket[expand-pipeline-arguments] or @racket[syntax-parameterize].
}

@defform[(expand-pipeline-arguments)]{
TODO - document this.
}

@subsection{Inspecting Pipelines}
@defthing[#:kind "procedure" pipeline? procedure?]{
Same as @racket[shell/mixed-pipeline/pipeline?]
}
@defthing[#:kind "procedure" pipeline-success? procedure?]{
Same as @racket[shell/mixed-pipeline/pipeline-success?]
}
@defthing[#:kind "procedure" pipeline-wait procedure?]{
Same as @racket[shell/mixed-pipeline/pipeline-wait]
}
@defthing[#:kind "procedure" pipeline-return procedure?]{
Same as @racket[shell/mixed-pipeline/pipeline-return]
}
@defthing[#:kind "procedure" pipeline-start-ms procedure?]{
Same as @racket[shell/mixed-pipeline/pipeline-start-ms]
}
@defthing[#:kind "procedure" pipeline-end-ms procedure?]{
Same as @racket[shell/mixed-pipeline/pipeline-end-ms]
}


@subsection{short names}
@defmodule[shell/pipeline-macro-short-names]

I don't want to commit to these short names being stable yet.  I may want to use a slightly different unix pipe, for instance, as the | pipe.

But they probably won't change much.

@defform[#:kind "pipeline-operator" (\| args ...+)]{
Alias for @racket[=default-unix-pipe=].

Note that the backslash is required in the normal racket reader because | is normally treated specially.  In the Rash reader, you can get this by typing just @bold{|}.
}
@defform[#:kind "pipeline-operator" (\|> args ...+)]{
Alias for @racket[=object-pipe=].

Note that the backslash is required in the normal racket reader because | is normally treated specially.  In the Rash reader, you can get this by typing just @bold{|>}.
}
@defform[#:id _ _]{
Alias for @racket[current-pipeline-argument].  It's an underscore, if you're having trouble telling which of the many horizontal line characters it is since it's all alone right there in that bar.
}
