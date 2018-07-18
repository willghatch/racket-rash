#lang scribble/manual

@(require
(for-label
racket/base
racket/contract
(except-in shell/mixed-pipeline run-pipeline)
(only-in shell/pipeline run-subprocess-pipeline
                        [pipeline? shell/pipeline/pipeline?])
(only-in shell/pipeline-macro run-pipeline)
))

@title[#:tag "mixed-pipeline"]{Mixed Unix-style and Racket Object Pipelines}

@defmodule[shell/mixed-pipeline]

@section{shell/mixed-pipeline stability}

This library is not entirely stable.

Some specific things that may change are the names of keyword arguments to run-mixed-pipeline, and the type of arguments and exact semantics of the redirection options for pipelines.

@section{shell/mixed-pipeline guide}

This is the runtime library behind
@secref["pipeline-macro" #:doc '(lib "shell/scribblings/shell-pipeline.scrbl")].

Everything from this module is also provided by the pipeline-macro library.  This library is primarily of interest for its functions for inspecting pipeline objects that represent running (or finished) pipelines.

@section{shell/mixed-pipeline reference}

@defproc[(run-mixed-pipeline [member (or/c unix-pipeline-member-spec?
                                     object-pipeline-member-spec?
                                     composite-pipeline-member-spec?)]
                       ...
[#:in in (or/c input-port? false/c) (open-input-string "")]
[#:out out any/c default-output-transformer]
@; TODO - fix this when I fix the contract in the implementation
@;[#:out out (or/c port? false/c path-string-symbol?
@;                (list/c path-string-symbol? (or/c 'append 'truncate 'error)))
@;                (current-output-port)]
[#:err err (or/c port? false/c path-string-symbol?
                (list/c path-string-symbol? (or/c 'append 'truncate 'error)))
                'string-port]
[#:strictness strictness (or/c 'strict 'lazy 'permissive) 'lazy]
[#:lazy-timeout lazy-timeout real? 1]
[#:return-pipeline-object return-pipeline-object any/c #f]
[#:bg bg any/c #f])
any/c]{

Run a pipeline potentially mixed with object and byte-stream components.

The pipeline members are run by a driver thread until they are completed.  Object pipeline members are run serially, while unix pipeline members are run in parallel.  When it is time to run a unix pipeline member, all unix members adjacent in a pipeline (including within composite pipeline members) are run in parallel with @racket[run-subprocess-pipeline].  If they are followed by an object member, the output port of the last unix member is passed to the object member and it starts running immediately.

If either @racket[bg] or @racket[return-pipeline-object] are non-false values, then the pipeline object itself is returned.  Otherwise the result of the final pipeline member is returned.  If there is an error in the pipeline and either @racket[bg] or @racket[return-pipeline-object] are true, the pipeline object is still returned (and can be checked for errors), otherwise the exception encountered in the pipeline is raised.  If @racket[bg] is non-false, then @racket[run-mixed-pipeline] returns the pipeline object immediately, otherwise @racket[run-mixed-pipeline] waits for the whole pipeline to finish before returning a value.

The @racket[in], @racket[out], and @racket[err] options only affect unix pipeline members.  Specifically, if the pipeline begins with a unix member, @racket[in] is used as its initial input port.  For all unix members that don't specify an error port, @racket[err] is used as their default.  If the last member of the pipeline is a unix member, then @racket[out] is used either as its output port, OR as a function that is appended to the pipeline to consume the final output port and produce some object.

The @racket[strictness] and @racket[lazy-timeout] options are also passed through to @racket[run-subprocess-pipeline], and only affect unix pipeline members.

This function is run by @racket[run-pipeline] macro, and otherwise this function should probably only be used if you want to write a replacement for the pipeline-macro library.

}

@subsection{Specifying Mixed Pipelines}

@defproc[(object-pipeline-member-spec? [spec any/c]) boolean?]{}
@defproc[(object-pipeline-member-spec [proc procedure?]) boolean?]{
@racket[proc] should be a function that accepts 0 arguments when used as the first member of a pipeline, and should be a function that accepts 1 argument when used as any other member of a pipeline.
}

@defproc[(unix-pipeline-member-spec? [spec any/c]) boolean?]{}
@defproc[(unix-pipeline-member-spec [argl any/c]
[#:err err (or/c port? false/c path-string-symbol?
                 (list/c path-string-symbol? (or/c 'append 'truncate 'error)))
                 hidden-default-value]
[#:success success-pred (or/c false/c procedure? (listof any/c)) hidden-default-value])
pipeline-member-spec?]{
This is the same as @racket[shell/pipeline/pipeline-member-spec].
}

@defproc[(composite-pipeline-member-spec? [spec any/c]) boolean?]{}
@defproc[(composite-pipeline-member-spec
[spec-list
(listof (or/c unix-pipeline-member-spec?
object-pipeline-member-spec?
composite-pipeline-member-spec?))])
boolean?]{
Creates a composite pipeline member spec.  This is essentially a convenience so that a user-facing function/macro for creating what looks like a single pipeline member can actually desugar into multiple pipeline stages.
}

@subsection[#:tag "inspecting-mixed-pipelines"]{Inspecting Mixed Pipelines}

@defproc[(pipeline? [pline any/c]) boolean?]{
Returned by @racket[run-mixed-pipeline] depending on its arguments.  Note that this is not the same as @racket[shell/pipeline/pipeline?].

Also, pipelines are synchronizable with the @racket[sync] function.
}

@defproc[(pipeline-success? [pline pipeline?]) boolean?]{
True if the pipeline had no errors, otherwise false.

This also waits for the pipeline to finish if it hasn't yet.
}

@defproc[(pipeline-wait [pline pipeline?]) any/c]{
Wait for the pipeline to finish.

@racket[(pipeline-wait pline)] is essentially the same as @racket[(sync pline)].
}

@defproc[(pipeline-return [pline pipeline?]) any/c]{
Returns the return value of the pipeline if it was successful, or the exception raised within it if the pipeline was unsuccessful.

This also waits for the pipeline to finish if it hasn't yet.
}

