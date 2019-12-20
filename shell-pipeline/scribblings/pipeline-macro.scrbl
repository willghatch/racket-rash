#lang scribble/manual

@(require
(for-label shell/pipeline-macro
           (except-in racket/base _)
           racket/list
           (only-in shell/demo/more-pipeline-operators =map= =filter=)
           @;(except-in shell/mixed-pipeline run-pipeline)
           ))

@; TODO - naming...
@title[#:tag "pipeline-macro"]{Pipeline Macro Library}

@defmodule[shell/pipeline-macro]

@section{shell/pipeline-macro stability}

Unstable features are flagged in the documentation.  There are few of them.

The base set of pipeline operators is likely to change, and some of the names I want to review before a stable release.

Some pipeline options (&in, etc) are currently backed by syntax parameters, which is wrong.  It causes things to be... not as lexical as they are supposed to be.  That will be changed.

@section{shell/pipeline-macro guide}

This module is a macro DSL wrapper around the
@secref["mixed-pipeline" #:doc '(lib "shell/scribblings/shell-pipeline.scrbl")]
library.  It is designed for running pipelines of external processes (which pass each other ports) and Racket functions (which pass each other objects).  It does this with a very flat syntax and user-definable pipeline operators, which provide a lot of convenient sugar for making pipelines shorter.  It is particularly tailored for use in a line-based syntax, like that of the Rash language.

Here are some quick examples:

@codeblock{
;; Pipe the output of ls to grep.
(run-pipeline =unix-pipe= ls -l =unix-pipe= grep foo)

;; To save on space, let's assume % is bound to =unix-pipe=
(run-pipeline % ls -l % grep foo)
}

We can also pipeline objects.  Object pipelines are full of functions instead of process specifications.

@codeblock{
;; This will return 3
(run-pipeline =object-pipe= list 0 1 2 =object-pipe= length)

;; To save on space, let's assume %> is bound to =object-pipe=
(run-pipeline %> list 0 1 2 %> length)
}

We can mix the two:

@codeblock{
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
                     )]]{
Run a pipeline.  This is a macro wrapper for @racket[shell/mixed-pipeline/run-mixed-pipeline] that uses pipeline operator macros to specify the pipeline to be run.  So you should read the docs about that as well.

The pipeline flags affect the options passed to @racket[shell/mixed-pipeline/run-mixed-pipeline] and are documented separately.

The pipeline-member-specs are transformed according to the pipeline operators given.  If the first non-flag argument to @racket[run-pipeline] is not a pipeline operator, then a default is put in its place as determined by a lexical default operator.  The default is determined by the lexical context of the first form among the arguments (after the run-pipeline identifier itself).  The full names of pipeline operators are conventionally identifiers surrounded with = signs.

At the time of writing I'm not really sure what to write here, so have an example:

@codeblock{(run-pipeline =object-pipe= list 1 2 3
                         =map= + 1 current-pipeline-argument
                         =map= + 1)}

This returns @(racketblock (list 3 4 5)).  Notice that @racket[current-pipeline-argument] is placed automatically for the second =map= operator -- it could have been left off of the other one as well, but I wanted to show what identifier is sneakily added.

If we instead run
@codeblock{
(run-pipeline =object-pipe= list 1 2 3
              =map= + 1 current-pipeline-argument
              =map= + 1
              &bg)
}
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

@racket[&<], @racket[&>], @racket[&>>], and @racket[&>!] each take a file name and cause (respectively) input redirection from the given file, output redirection to the given file erroring if the file exists, output redirection appending to the given file, and output redirection truncating the given file.  @racket[&in], @racket[&out], and @racket[&err] take an argument suitable to be passed to #:in, #:out, and #:err of @racket[shell/mixed-pipeline/run-mixed-pipeline].

@racket[&bg] and @racket[&pipeline-ret] toggle #:bg and #:return-pipeline-object, and @racket[&strict], @racket[&permissive], and @racket[&lazy] set the #:strictness argument.
@bold{
Not stable:
All of the file redirection flags may change how they do things like dollar, glob, and tilde expansion.
}
}


@defform[#:literals (with-pipeline-config)
         (with-pipeline-config option ... body ...)
         #:grammar [(option
                     (code:line #:in in-expr)
                     (code:line #:out out-expr)
                     (code:line #:err err-expr)
                     (code:line #:starter default-pipeline-starter))
                    ]]{
Parameterizes the body forms to use @racket[default-pipeline-starter] when no starter is explicitly given, and to use @racket[in-expr], @racket[out-expr], and @racket[err-expr] as default redirections.  Note that the expressions given for redirection are evaluated for every pipeline run in the body.
}
@defform[#:literals (splicing-with-pipeline-config)
         (splicing-with-pipeline-config option ... body ...)
         #:grammar [(option
                     (code:line #:in in-expr)
                     (code:line #:out out-expr)
                     (code:line #:err err-expr)
                     (code:line #:starter default-pipeline-starter))
                    ]]{
Like @racket[with-pipeline-config], except that it splices into definition contexts.
}


@subsection{Pipeline Operators}

The core module only provides a few simple pipeline operators.  There are many more in the demo/ directory in the source repository.  Most of them are hastily written experiments, but some good ones should eventually be standardized.

@defform[#:kind "pipeline-operator" (=composite-pipe= (pipe-op arg ...) ...+)]{
Produces a composite pipeline member spec made from the pipeline operators given.  This is really more for use when defining new pipeline operators than for use in a pipeline itself.
}

@defform[#:kind "pipeline-operator" (=basic-unix-pipe= options ... args ...+)]{
Produces a unix-pipeline-member-spec with the given arguments as the process/function argument list.  The arguments are not quoted.  Any argument that produces a list will be spliced into the argument list.

Options all take an argument, must precede any arguments, and are as follows:

@racket[#:err] - Takes an expression to produce an error redirection value suitable for @racket[unix-pipeline-member-spec].

@racket[#:success] - Takes an expression suitable for the @racket[#:success] argument of @racket[unix-pipeline-member-spec].

TODO - env modification

Arguments are evaluated left-to-right, and the first argument (that does not produce an empty list) is checked to be a valid executable path.  If it is not a valid executable path, an exception is raised before evaluating the other arguments.  (Note that an executable could be removed between a successful check and an attempt to spawn a subprocess, so it could still error after evaluating the other arguments.)

}

@defform[#:kind "pipeline-operator" (=unix-pipe= arg ...+)]{
This is the pipe that does more or less what you expect.  It does tilde expansion (~ -> $HOME).  It does globbing.  When you have $identifiers-with-dollar-signs they are expanded into variable references.  When $DOLLAR_IDENTIFIERS_ARE_CAPITALIZED they are expanded to environment variable lookups.

The operator quotes other arguments, and then passes through to @racket[=basic-unix-pipe=].  But it takes some extra optional arguments:

@racket[#:as] - This is sugar for adding on an object pipeline member afterward that parses the output somehow.  This should be given either #f (no transformation), a port reading function (eg. @racket[port->string]), or one of a pre-set list of symbols:  @racket['string], @racket['trim], @racket['lines], or @racket['words].

@racket[#:e>] - Accepts a file name (as an identifier), redirects the error stream to that file.  Produces an error if the file exists.

@racket[#:e>!] - Accepts a file name (as an identifier), redirects the error stream to that file.  Truncates the file if it exists.

@racket[#:e>>] - Accepts a file name (as an identifier), redirects the error stream to that file.  Appends to the file if it exists.


BUT: if the first argument is a pipeline alias defined with @racket[define-pipeline-alias] or @racket[define-simple-pipeline-alias], then the operator from that alias is swapped in instead, skipping everything else that this operator would normally do.

@codeblock|{
(run-pipeline =unix-pipe= echo $HOME/*.rkt)
(define-simple-pipeline-alias d 'ls '--color=auto)
(define dfdir 'dotfiles)
(run-pipeline =unix-pipe= d $HOME/$dfdir)
}|

At runtime, the first argument is evaluated and tested to see if it exists as an executable on the path.  If it's not, an error is raised before the other arguments are evaluated.  (This is an optimization for error messages -- the executable could still be removed between evaluating other arguments and executing the program, in which case the error message is worse.)

Usually @racket[\|] is used instead.

@bold{
Unstable:

The way dollar, tilde, and glob expansion is done by default may change (though it will still be consistent with examples in the GPCE paper).

The #:as flag may go away (it's redundant with just piping to a reader function).
}
}
@defform[#:kind "pipeline-operator" (\| arg ...+)]{
Alias for @racket[=unix-pipe=].

Note that the backslash is required in the normal racket reader because @bold{|} is normally treated specially.  In the Rash reader, you can get this by typing just @bold{|}.
}

@defform[#:kind "pipeline-operator" (=basic-object-pipe/expression= e)]{
The simplest object pipe.  @racket[e] is simply the body of a @racket[lambda].  When used as a pipeline starter, the lambda accepts no arguments.  Otherwise it is a single-argument function, and @racket[current-pipeline-argument] is used to refer to its argument.

@bold{
Unstable.  The name will probably change.
}
}

@defform[#:kind "pipeline-operator" (=basic-object-pipe/form= arg ...+)]{
Creates an object pipe where @code{(arg ...)} is the body of a function.

As with other object pipes, when used as a pipeline starter it generates a lambda with no arguments, and as a pipeline joint it generates a lambda with one argument, @racket[current-pipeline-argument].

@bold{
Unstable.  The name will probably change.
}
}

@defform[#:kind "pipeline-operator" (=basic-object-pipe= f-arg arg ...)]{
Like @racket[=basic-object-pipe/form=], except that when not used as a pipeline starter, if the @racket[current-pipeline-argument] is not used within the arguments, it is appended as the last argument.

To discover whether @racket[current-pipeline-argument] is used, each argument is local-expanded.  So @racket[f-arg] must evaluate to function, and NOT be the name of a macro to be expanded with the other args.

@bold{Note:}  If @racket[=basic-object-pipe=] follows a subprocess form, it should close the received port.  Operators that handle ports automatically such as @racket[\|>>] close the port automatically, but @racket[\|>] allows things like lazy reading into a stream, so it can not apply an automatic policy that will always work effectively.

Usually @racket[\|>] is used instead.

@bold{
Unstable.  The name will probably change and/or this name may be given to a pipe of different meaning.  But the @racket[\|>] alias will NOT have its meaning changed (IE it is stable).
}
}
@defform[#:kind "pipeline-operator" (\|> arg ...+)]{
Alias for @racket[=basic-object-pipe=].

Note that the backslash is required in the normal racket reader because @bold{|} is normally treated specially.  In the Rash reader, you can get this by typing just @bold{|>}.

@bold{
Unstable.  The name will probably change.
}
}

@defform[#:kind "pipeline-operator" (=object-pipe/expression= arg ...+)]{
Like @racket[=basic-object-pipe/expression=], but when it receives a port as an argument, it converts it to a string.

@bold{
Unstable.  The name will probably change.
}
}
@defform[#:kind "pipeline-operator" (=object-pipe/form= arg ...+)]{
Like @racket[=basic-object-pipe/form=], but when it receives a port as an argument, it converts it to a string.

@bold{
Unstable.  The name will probably change.
}
}
@defform[#:kind "pipeline-operator" (=object-pipe= f-arg arg ...)]{
Like @racket[=basic-object-pipe=], but when it receives a port as an argument, it converts it to a string.

The same caveat applies that @racket[f-arg] must NOT be a macro you expect to expand with the other arguments -- it must evaluate to a function.

Usually @racket[\|>>] is used instead.

@bold{
Unstable.  The name will probably change.  But note that the @racket[\|>>] alias is stable.
}
}
@defform[#:kind "pipeline-operator" (\|>> arg ...+)]{
Alias for @racket[=object-pipe=].

Note that the backslash is required in the normal racket reader because @bold{|} is normally treated specially.  In the Rash reader, you can get this by typing just @bold{|>>}.
}

@defidform[#:kind "pipeline-operator"
         default-pipeline-starter]{
Syntax parameter determining which pipeline operator is inserted when a @racket[run-pipeline] form doesn't explicitly start with one.

@bold{
Unstable in that it will not be a syntax parameter in the future, but something with better semantics for what it's supposed to do.
}
}

I've written various other pipeline operators that are a little more exciting and that are currently in the demo directory of the repository.  I'll eventually polish them up and put them somewhere stable.  They include things like unix pipes that automatically glob things, unix pipes that have lexically scoped alias resolution, =filter=, =map=, =for/stream=,=for/list/unix-arg=,=for/list/unix-input=...

@subsection{Defining Pipeline Operators}

@defform[(define-pipeline-operator name start-or-joint ...)
#:grammar
[(start-or-joint
(code:line #:start transformer)
(code:line #:joint transformer)
(code:line #:operator transformer))
]]{
Define a pipeline operator.  Pipeline operators can act differently when they are in the starting position of a pipeline or later (joint position).  Specifically, when an operator creates an @racket[object-pipeline-member-spec], it needs to have a function that accepts 0 arguments when in the start position and 1 argument in others.

If you use the #:operator keyword, the same transformer is used in start and joint positions.

If a transformer function is not specified for one of the options, a default implementation (that generates an error) is used.

The transformer will receive a syntax object corresponding to @code{(name-of-pipe argument ...)}, so it will likely want to ignore its first argument like most macros do.  But simetimes it may be useful to recur.

If a pipeline operator is used outside of @racket[run-pipeline], it raises a syntax error.

The operator must desugar to code which produces a pipeline-member-spec.

Example uses are in the demo directory in the repository.

@bold{
Unstable:

Currently for operators to desugar to another operator, the new operator must be at the top-level of the returned syntax or it will raise an error (IE that a pipeline operator was used out of context).  This should be fixed later.
}
}


@;@defform[(pipeop name syntax-parser-clauses ...+)]{
@;The name of this will probably change.  And maybe it will go away entirely.  I'm not sure yet.
@;
@;@racket[pipeop] is a more streamlined version of @racket[define-pipeline-operator].  It defines a pipeline operator where both @racket[#:start] and @racket[#:joint] are the same.  The syntax transformer used is basically @code{(syntax-parser syntax-parser-clauses ...)}.  I made this because I thought it would be a convenient way to be able to swiftly define a new pipeline even interactively in the Rash repl.
@;
@;Example uses are in the demo directory.
@;}


@defform[(define-pipeline-alias name transformer)]{
Defines an alias macro recognized by @racket[=unix-pipe=] and maybe others.

@racket[transformer] must be a syntax transformer function, and must return a syntax object that starts with a pipeline operator.

@codeblock|{
;; Unix `find` has to take the directory first, but we want
;; to always add the -type f flag at the end.
(define-pipeline-alias find-f
  (syntax-parser
    [(_ arg ...) #'(=unix-pipe= find arg ... -type f)]))

;; these are equivalent
(run-pipeline =unix-pipe= find-f ".")
(run-pipeline =unix-pipe= find "." -type f)
}|

@bold{
Unstable:  I might re-think this slightly...  if it changes it will be similar and existing aliases will probably still work, but I'm not 100% sure so I want to hedge.
}
}

@defform[(define-simple-pipeline-alias name cmd arg ...)]{
Simple sugar for @racket[define-pipeline-alias].  It defines an alias transformer that uses @racket[=unix-pipe=].

@codeblock|{
(define-simple-pipeline-alias ls 'ls '--color=auto)
;; these are equivalent
(run-pipeline =unix-pipe= d -l $HOME)
(run-pipeline =unix-pipe= 'ls '--color=auto -l $HOME)
}|

Note that arguments are passed through as-is to @racket[=unix-pipe=], so if you use the name of another alias it will cause alias replacement a second time unless you quote the command name or put it in a string.

@bold{
Unstable:  The name might change or something.  I just want to hedge the possibility that I want to re-look at aliases.
}
}

@defform[#:id current-pipeline-argument current-pipeline-argument]{
The name of the implicit argument for object pipes.  The default is an error, and pipe operators that accept it must set it up using @racket[expand-pipeline-arguments] or @racket[syntax-parameterize].

Usually @racket[_] is used instead.
}
@defform[#:id _ _]{
Alias for @racket[current-pipeline-argument].  It's an underscore, if you're having trouble telling which of the many horizontal line characters it is since it's all alone right there in that bar.
}

@defform[(expand-pipeline-arguments)]{
TODO - document this.

It's used to get the @racket[current-pipeline-argument] detected and inserted automatically if missing.

@bold{
Unstable -- this is not a great API and I don't want the burden of maintaining it.
}
}

@subsection{Inspecting Pipelines}
See @secref{inspecting-mixed-pipelines}.


@section{Demo stuff reference}
@(declare-exporting shell/demo/more-pipeline-operators)
These things are documented in the Rash documentation, but I'm adding these definitions to not have broken links...

You can get these with @verbatim{(require rash/demo/setup)}

@defform[#:kind "pipeline-operator" (=map= arg ...)]{Unstable in that it will probably be moved into the main rash module, but I think it will stay the same now.}
@defform[#:kind "pipeline-operator" (=filter= arg ...)]{Unstable in that it will probably be moved into the main rash module, but I think it will stay the same now.}
