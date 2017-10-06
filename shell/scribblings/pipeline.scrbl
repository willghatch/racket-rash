#lang scribble/manual

@(require (for-label racket/base
                     racket/contract
                     shell/pipeline))

@title[#:tag "pipeline"]{Basic Unix-style Pipelines}

@defmodule[shell/pipeline]

@section{shell/pipeline stability}

This library is not entirely stable.

Forthcoming features include features such as process redirection (similar to @bold{<()} and @bold{>()} in Bash).

Some specific things that may change are the names of keyword arguments to run-pipeline, and the type of arguments and exact semantics of the redirection options for pipelines.  Also, the extra run-pipline/ functions.

@section{shell/pipeline guide}

@; TODO - I should probably just rewrite this, but at least I should make sure it all makes sense within the tower of libraries to Rash.  And I should maybe explain better the types of return values you get from functions, or the defaults they have...
This library makes unix-style pipelines of external programs and racket functions easy.  You can write things as simply as @code{(run-pipeline '(cat /etc/passwd) '(grep root) '(cut -d : -f 1))}, which will print "root\n" to stdout (on unix systems) and will return a pipeline object.  To get the output as a string, use @racket[run-pipeline/out] the same way.  You can also put racket functions in the pipeline.  If you have a racket implementation of grep called my-grep, you can do @code{(run-pipeline '(cat /etc/passwd) `(,my-grep root) '(cut -d : -f 1))} to get the same results.  So you can write all sorts of filter functions in Racket rather than using shell commands.

Symbols in pipelines are turned into strings before they are passed in as arguments to subprocesses.  Arguments to racket functions are not transformed in any way.

This library DOES work on MS Windows, and if it can't find a program it retries the name with a .exe at the end.  But Microsoft doesn't seem to believe in having useful shell utilities, or in putting program executables on the PATH, or adding program locations to the PATH.  So it will probably still be more useful on Unix than on Windows.


@section{shell/pipeline reference}

@defproc[(run-pipeline [member (or/c list? pipeline-member-spec?)] ...
[#:in in (or/c input-port? false/c) (current-input-port)]
[#:out out (or/c port? false/c path-string-symbol?
                (list/c path-string-symbol? (or/c 'append 'truncate 'error)))
                (current-output-port)]
[#:err err (or/c port? false/c path-string-symbol?
                (list/c path-string-symbol? (or/c 'append 'truncate 'error)))
                (current-error-port)]
[#:strictness strictness (or/c 'strict 'lazy 'permissive) 'lazy]
[#:lazy-timeout lazy-timeout real? 1]
[#:background? bg? any/c #f])
any/c]{
Run a pipeline.  Each @racket[member] should be either a @racket[pipeline-member-spec] or a list, where the first of the list is the command and the rest are arguments.  The command may be a symbol, string, path, or function.  If it is a string or path, it will spawn a subprocess.  If it is a function, it will use that function in a thread.  If the command is an @racket[alias-func], it is called with its arguments before the pipeline is started to receive a new command/argument list which replaces it.

A @racket[pipeline-member-spec], in addition to the command/argument list, has an error-port specification.  All lists given will be turned into @racket[pipeline-member-spec]s using the @racket[err] specification.

Each member of the pipeline will have its @racket[current-output-port] connected to the @racket[current-input-port] of the next member.  The first and last members use @racket[in] and @racket[out], respectively, to communicate with the outside world.

All ports specified (@racket[in], @racket[out], @racket[err]) may be either a port, the symbol @code{'null}, #f, or a path/string/symbol.  The error port may be @code{'stdout}, in which case the output port will be used.  If #f is given, then a port will be returned in the pipeline struct returned (similar to @racket[subprocess]).  If @code{'null} is given a null output port or empty string port is used.  If a path/string/symbol is given, then a file at that path is opened.

Output and error ports may be a list of a path/string/symbol and any of @code{'error}, @code{'append}, or @code{'truncate} to specify what should happen if the file already exists.

Beware that just as with @racket[subprocess], if you pass #f to get an input, output, or error port out of a pipeline, the resulting port may be a file-stream-port, and you will need to be sure to close it.  Otherwise all file-stream-port handling in the pipeline and for file redirection is done automatically.

@racket[strictness] determines how success is reported.  If @racket[strictness] is @code{'strict}, then the pipeline is successful when all members are successful, and if there are errors the first member to have an error is reported.  If @racket[strictness] is @code{'lazy}, success is similar, but treats any members that were killed as successful.  If @racket[strictness] is @code{'permissive}, then errors are ignored except for the last pipeline member, which is what bash and most other shell languages do.

Also, if @racket[strictness] is @code{'lazy} or @code{'permissive}, then when a pipeline member finishes, pipeline members before it may be killed.  In permissive mode they may be killed immediately, and in lazy mode they have @racket[lazy-timeout] seconds to finish before they are killed.  This process killing happens to not wait for long (potentially infinitely so) processes in the middle of a pipeline when only a small part of their output is used.  For instance, piping the output of a large file (or cat-ing an infinite pseudo-file) to the "head" command.  This mirrors what bash and other shells do.

If @racket[background?] is false, then @racket[run-pipeline] uses @racket[pipeline-wait] to wait until it finishes, then returns the status with @racket[pipeline-status].  If @racket[background?] is not false, then @racket[run-pipeline] returns a @racket[pipeline] object.
}

@defproc[(run-pipeline/out [member (or/c list? pipeline-member-spec?)] ...
[#:in in (or/c input-port? false/c path-string-symbol?) (open-input-string "")]
[#:status-and? status-and? any/c #f])
any/c]{
Like @racket[run-pipeline], but string-ports are used as the input, output, and error ports.  It does not return until the pipeline finishes, and returns the output string.  If the pipeline has an unsuccessful status, an exception is raised (with the contents of the error port).
}

@defproc[(pipeline-member-spec? [pmspec any/c]) boolean?]{
Is it a pipeline-member-spec?
}
@defproc[(pipeline-member-spec [argl any/c]
[#:err err (or/c port? false/c path-string-symbol?
                 (list/c path-string-symbol? (or/c 'append 'truncate 'error)))
                 hidden-default-value]
[#:success success-pred (or/c false/c procedure? (listof any/c)) hidden-default-value])
pipeline-member-spec?]{
Make a pipeline-member-spec.  @racket[argl] is the command/argument list.  The first value in the list is the command, and should either be a @racket[path-string-symbol?] to a command or a function.
@racket[err] is the error port specification for it to use.
@racket[success-pred] is a predicate that will be applied to the return value of a pipeline member to determine its success.  Subprocesses will be considered successful if they return 0 when @racket[success-pred] is #f, or if @racket[success-pred] is a list, if they return 0 or a member of that list, or if @racket[success-pred] is a function, it will be successful if @code{(success-pred return-value)} returns a non-#f value.  Function pipeline members are always considered unsuccessful if they throw an error.  Otherwise, they are successful if @racket[success-pred] is #f, if their return is a member of @racket[success-pred] when it is a list, or when @code{(success-pred return-value)} is true when it is a function.

@racket[err] and @racket[success-pred] default to values that can be overridden by the defaults set by the pipeline-running functions.  But in the end they default to current-error-port and #f.
}

@defproc[(pipeline? [p any/c]) boolean?]{
Is it a pipeline object?
}

@defproc[(pipeline-port-to [p pipeline?]) (or/c false/c output-port?)]{Get initial input port (if one was provided initially, this will be false)}
@defproc[(pipeline-port-from [p pipeline?]) (or/c false/c input-port?)]{Get final output port (if one was provided initially, this will be false)}
@defproc[(pipeline-err-ports [p pipeline?]) (listof (or/c false/c input-port?))]{Get list of error ports for the pipeline (each one that was provided will be false)}
@defproc[(pipeline-wait [p pipeline?]) void?]{Wait for the pipeline to finish.  If the pipeline strictness is permissive, then a pipeline is finished when the ending member of the pipeline is finished.
Pipelines are also synchronizable events that are ready for synchronization when the pipeline is finished.
}
@defproc[(pipeline-kill [p pipeline?]) void?]{Kill a running pipeline.}
@defproc[(pipeline-running? [p pipeline?]) boolean?]{Is the pipeline currently running?}
@defproc[(pipeline-success? [p pipeline?]) any/c]{Waits for the pipeline to terminate (according to @racket[pipeline-wait]).  Returns #t if the pipeline was considered successful, else #f.
If the strictness argument is 'strict or 'lazy, then all members must succeed.  If it is 'permissive then only the last one must succeed.
A pipeline member is considered successful if it was a subprocess and returned 0, or if it was a thread and raised no uncaught exceptions.
If strictness is lazy then pipeline members that were killed are also considered successful.}
@defproc[(pipeline-status [p pipeline?]) any/c]{Waits for the pipeline to terminate (according to @racket[pipeline-wait]).  Returns the status of the pipeline.
If the strictness was 'strict or 'lazy, then the status will be the status of the first unsuccessful member or the status of the last member.
If the strictness was 'permissive, then the status will be the status of the last member.
The status of any member is its return code for a process, the return of or exception thrown by the function of a thread member, or 'killed if it was killed.
}
@defproc[(pipeline-status/list [p pipeline?]) (listof any/c)]{A list of the exit statuses of all the pipeline members.}


@defproc[(shellify [func procedure?]) procedure?]{
Convenience function for putting Racket functions into pipelines.

Takes a procedure which takes a string as its first argument and returns a string.  Returns a procedure which will turn its @racket[current-input-port] into a string and pass it to the original procedure as its first argument.  It then displays the output string of the function to its @racket[current-output-port].
}

@defthing[prop:alias-func]{
Struct property for alias-funcs.  The property should be a function that takes the structure as an argument and produces a function that takes the argument list and produces a new one.
}
@defstruct[alias-func ([func procedure?])]{
Wrapper struct with @racket[prop:alias-func].  An alias function must return a non-empty list suitable for a @racket[pipeline-member-spec].

Examples:
@codeblock|{
;; A simple case -- have an alias that sets initial arguments.
(define ls-alias (alias-func (λ args (list* 'ls '--color=auto args))))
;; Slightly more complicated: `find` requires that its path argument go before
;; its modifier flags.
(define find-files-alias (alias-func (λ args `(find ,@args -type f))))
}|
}

@defproc[(path-string-symbol?
[p any/c])
boolean?]{
Like @racket[path-string?], except it also includes symbols that would be valid paths.
}

@defform[(and/success e ...)]{
Like @racket[and], but only treats pipeline objects as truthy if they pass @racket[pipeline-success?].
}
@defform[(or/success e ...)]{
Like @racket[or], but only treats pipeline objects as truthy if they pass @racket[pipeline-success?].
}
