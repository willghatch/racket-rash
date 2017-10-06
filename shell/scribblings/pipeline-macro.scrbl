#lang scribble/manual

@(require
(for-label shell/pipeline-macro
           (prefix-in shell/mixed-pipeline/ shell/mixed-pipeline)
           ))

@; TODO - naming...
@title[#:tag "pipeline-macro"]{Pipeline Macro Library}

@defmodule[shell/macro-pipeline]

@section{shell/macro-pipeline stability}

This library is not entirely stable.

The base set of pipeline operators is likely to change, and some of the names I want to review before a stable release.

@section{shell/macro-pipeline guide}

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

@section{shell/macro-pipeline reference}

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

TODO - document the core operators

@;   TODO - some of these do more than they ought -- some of the basic ones do too much, maybe some should be split into a second file with more adveturous operators...
@;   default-pipeline-starter
@;   =composite-pipe=
@;   =basic-object-pipe=
@;   =basic-object-pipe/left=
@;   =basic-object-pipe/expression=
@;   =object-pipe=
@;   =object-pipe/left=
@;   =object-pipe/expression=
@;
@;   =basic-unix-pipe=
@;   =quoting-basic-unix-pipe=

@subsection{Defining Pipeline Operators}

TODO - document the defining forms

@;   define-pipeline-operator
@;   pipeop
@;
@;   current-pipeline-argument
@;   expand-pipeline-arguments
@;


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

