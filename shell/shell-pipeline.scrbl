#lang scribble/manual

@(require (for-label racket/base
                     racket/contract
                     shell/pipeline))

@title[#:tag "shell-pipeline"]{Shell Pipeline Library}
@author+email["William Hatch" "william@hatch.uno"]

@defmodule[shell/pipeline]

@section{Guide}

This library makes unix-style pipelines of external programs and racket functions easy.  You can write things as simply as @code{(run-pipeline '(cat /etc/passwd) '(grep root) '(cut -d : -f 1))}, which will print "root\n" to stdout (on unix systems) and will return 0.  To get the output as a string, use @racket[run-pipeline/out] the same way.  You can also put racket functions in the pipeline.  If you have a racket implementation of grep called my-grep, you can do @code{(run-pipeline '(cat /etc/passwd) `(,my-grep root) '(cut -d : -f 1))} to get the same results.  So you can write all sorts of filter functions in Racket rather than using shell commands.

Symbols in pipelines are turned into strings before they are passed in as arguments to subprocesses.  Arguments to racket functions are not transformed in any way, but my intention is that they should usually treat symbols as strings when reasonable.

Now go and write your shell scripts in Racket instead of the bourne shell, bash, zsh, ksh, csh, tcsh, ash, dash, fish, ...

This library DOES work on MS Windows, and if it can't find a program it retries the name with a .exe at the end.  But Microsoft doesn't seem to believe in having useful shell utilities, or in putting program executables on the PATH, or adding program locations to the PATH.  So it will probably still be more useful on Unix than on Windows.

This library is also intended to support another forthcoming library with a line-based syntax with arbitrary embedding of s-expression based Racket.


@section{Reference}

@defproc[(run-pipeline [member (or/c list? pipeline-member-spec?)] ...
[#:in in (or/c input-port? false/c) (current-input-port)]
[#:out out (or/c port? false/c path-string-symbol?
                (list/c path-string-symbol? (or/c 'append 'truncate 'error)))
                (current-output-port)]
[#:default-err default-err (or/c port? false/c path-string-symbol?
                (list/c path-string-symbol? (or/c 'append 'truncate 'error)))
                (current-error-port)]
[#:end-exit-flag end-exit-flag any/c #t]
[#:status-and? status-and? any/c #f]
[#:background? bg? any/c #f])
any/c]{
Run a pipeline.  Each @racket[member] should be either a @racket[pipeline-member-spec] or a list, where the first of the list is the command and the rest are arguments.  The command may be a symbol, string, path, or function.  If it is a string or path, it will spawn a subprocess.  If it is a function, it will use that function in a thread.  If it is a symbol, it will look up in @racket[current-shell-functions] and use the resulting function if one is found.  If no shell function is found, it will be converted into a string.  If the command (or the shell-function found by a lookup) is an @racket[alias-func], it is called to receive a new command/argument list which is resolved similarly.

A @racket[pipeline-member-spec], in addition to the command/argument list, has an error-port specification.  All lists given will be turned into @racket[pipeline-member-spec]s using the @racket[default-err] specification.

Each member of the pipeline will have its @racket[current-output-port] connected to the @racket[current-input-port] of the next member.  The first and last members use @racket[in] and @racket[out], respectively, to communicate with the outside world.

All ports specified (@racket[in], @racket[out], @racket[default-err]) may be either a port, the symbol @code{'null}, #f, or a path/string/symbol.  The error port may be @code{'stdout}, in which case the output port will be used.  If #f is given, then a port will be returned in the pipeline struct returned (similar to @racket[subprocess]).  If @code{'null} is given a null output port or empty string port is used.  If a path/string/symbol is given, then a file at that path is opened.

Output and error ports may be a list of a path/string/symbol and any of @code{'error}, @code{'append}, or @code{'truncate} to specify what should happen if the file already exists.

Beware that just as with @racket[subprocess], if you pass #f to get an input, output, or error port out of a pipeline, the resulting port may be a file-stream-port, and you will need to be sure to close it.  Otherwise all file-stream-port handling in the pipeline and for file redirection is done automatically.

If @racket[status-and?] is true, then the return status (or status given by @racket[pipeline-status]) will be the first unsuccessful status (nonzero) in the pipeline, or 0 if they are all successful.  Otherwise the status returned only reflects the last member of the pipeline (mirroring the behavior of most shell languages).

If @racket[end-exit-flag] is true and @racket[status-and?] is not, then when the last member of the pipeline finishes, all previous members will be killed.  This matches the behavior of pipelines in most shell languages (eg. try "find / | head -n 1").

If @racket[background?] is false, then @racket[run-pipeline] uses @racket[pipeline-wait] to wait until it finishes, then returns the status with @racket[pipeline-status].  If @racket[background?] is not false, then @racket[run-pipeline] returns a @racket[pipeline] object.
}

@defproc[(run-pipeline/out [member (or/c list? pipeline-member-spec?)] ...
[#:end-exit-flag end-exit-flag any/c #t]
[#:status-and? status-and? any/c #f])
any/c]{
Like @racket[run-pipeline], but string-ports are used as the input, output, and error ports.  It does not return until the pipeline finishes, and returns the output string.  If the pipeline has an unsuccessful status, an exception is raised (with the contents of the error port).
}

@defstruct[pipeline-member-spec
([argl (listof any/c)]
[port-err (or/c port? false/c path-string-symbol?
                (list/c path-string-symbol? (or/c 'append 'truncate 'error)))])]{
@racket[argl] is the command/argument list for a member of a pipeline.  @racket[port-err] is a specification for the error port to use -- just like in the default-err argument of @racket[run-pipeline].
}


@defproc[(pipeline? [p any/c]) boolean?]{
Is it a pipeline object?
}

@defproc[(pipeline-port-to [p pipeline?]) (or/c false/c output-port?)]{Get initial input port (if one was provided initially, this will be false)}
@defproc[(pipeline-port-from [p pipeline?]) (or/c false/c input-port?)]{Get final output port (if one was provided initially, this will be false)}
@defproc[(pipeline-err-ports [p pipeline?]) (listof (or/c false/c input-port?))]{Get list of error ports for the pipeline (each one that was provided will be false)}
@defproc[(pipeline-wait [p pipeline?]) void?]{Wait for the pipeline to finish.}
@defproc[(pipeline-kill [p pipeline?]) void?]{Kill a running pipeline.}
@defproc[(pipeline-running [p pipeline?]) boolean?]{Is the pipeline currently running?}
@defproc[(pipeline-status [p pipeline?]) any/c]{Returns the status of the pipeline.  If the pipeline was make with a true status-and argument, then it is the first nonzero exit status of the pipeline, otherwise it is the status of the last member of the pipeline.}
@defproc[(pipeline-status/list [p pipeline?]) (listof any/c)]{A list of the exit statuses of all the pipeline members.}


@defproc[(shellify [func procedure?]) procedure?]{
Convenience function for putting Racket functions into pipelines.

Takes a procedure which takes a string as its first argument and returns a string.  Returns a procedure which will turn its @racket[current-input-port] into a string and pass it to the original procedure as its first argument.  It then displays the output string of the function to its @racket[current-output-port] and returns 0.  If an exception is raised by the original function, its text will be output to its @racket[current-error-port], and it will return something other than 0.
}

@defparam[current-shell-functions table (hash/c symbol? procedure?)]{
Parameter that holds the current mapping for strings and symbols to look up shell functions (including aliases) before looking for an executable program.
}

@defthing[base-shell-functions
]{
Base table for @racket[current-shell-functions].  Includes a binding from @code{'cd} to @racket[shell-cd], @code{'printf} to @racket[shell-printf], and @code{'echo} to @racket[shell-echo].
}

@defproc[(add-shell-function
[name symbol?]
[shell-func procedure?])
void?]{
Adds @racket[shell-func] to current-shell-functions under @racket[name].  @racket[shell-func] should follow the rules for function members of pipelines.
}

@defproc[(shell-alias
[name symbol?]
[alias-list (listof any/c)])
void?]{
Makes an @racket[alias-func] and adds it to the current-shell-functions under @racket[name].  The alias func that is created appends any use-site arguments to the argument list in @racket[alias-list].  Basically like what @code{alias} does in @code{bash}.
}

@defstruct[alias-func ([func procedure?])]{
Wrapper struct with @racket[prop:procedure] for alias functions.  An alias function must return a non-empty list suitable for a @racket[pipeline-member-spec].
}

@defstruct[pipeline-same-thread-func ([func procedure?])
]{
Wrapper struct with @racket[prop:procedure].  If a @racket[pipeline-member-spec] has one of these as its command, it will be executed without spawning a new thread.  This is basically a hack to make @racket[shell-cd] work while being called in a pipeline.  Don't use this.
}

@defproc[(path-string-symbol?
[p any/c])
boolean?]{
Like @racket[path-string?], except it also includes symbols that would be valid paths.
}

@defproc[(shell-cd [dir (or/c string? path? symbol?) ""]) void?]{
Changes @racket[current-directory].  It's a @racket[pipeline-same-thread-func], so it changes the @racket[current-directory] for the current thread rather than a throwaway thread.  If no directory is given, it changes to the user's home directory.
}
@defproc[(shell-printf [format-string string?] [arg ang/c] ...) void?]{
Like normal printf, except it returns 0.
}
@defproc[(shell-echo [arg ang/c] ...) void?]{
Each argument is displayed to the @racket[current-output-port] with a space in between.  A newline is displayed after all arguments.  Returns 0.
}


@section{Code and License}

The code is available
@hyperlink["https://github.com/willghatch/racket-shell-pipeline"]{on github}.

This library is licensed under the terms of the LGPL version 3, or (at
your option) any later version published by the Free Software
Foundation (IE LGPL3+).
