#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     shell/pipeline
                     (prefix-in shell/mixed-pipeline/ shell/mixed-pipeline)
                     (prefix-in shell/pipeline-macro/ shell/pipeline-macro)
                     ))

@title[#:tag "pipeline"]{Basic Unix-style Pipelines}

@defmodule[shell/pipeline]

@section{shell/pipeline stability}

Unstable features are flagged in the documentation.  There are a few of them, but they are mostly on functions you are not likely to use.

Forthcoming features include features such as process redirection (similar to @bold{<()} and @bold{>()} in Bash).


@section{shell/pipeline guide}

@; TODO - I should probably just rewrite this, but at least I should make sure it all makes sense within the tower of libraries to Rash.  And I should maybe explain better the types of return values you get from functions, or the defaults they have...
This library makes unix-style pipelines of external programs and racket functions easy.  You can write things as simply as @code{(run-subprocess-pipeline '(cat /etc/passwd) '(grep root) '(cut -d : -f 1))}, which will print "root\n" to stdout (on unix systems) and will return a pipeline object.  To get the output as a string, use @racket[run-subprocess-pipeline/out] the same way.  You can also put racket functions in the pipeline.  If you have a racket implementation of grep called my-grep, you can do @code{(run-subprocess-pipeline '(cat /etc/passwd) `(,my-grep root) '(cut -d : -f 1))} to get the same results.  So you can write all sorts of filter functions in Racket rather than using shell commands.

Symbols in pipelines are turned into strings before they are passed in as arguments to subprocesses.  Arguments to racket functions are not transformed in any way.

This library DOES work on MS Windows, and if it can't find a program it retries the name with a .exe at the end.  But Microsoft doesn't seem to believe in having useful shell utilities, or in putting program executables on the PATH, or adding program locations to the PATH.  So it will probably still be more useful on Unix than on Windows.


@section{shell/pipeline reference}

@defproc[(run-subprocess-pipeline [member (or/c list? pipeline-member-spec?)] ...
[#:in in (or/c input-port? false/c special-redirect?) (current-input-port)]
[#:out out (or/c port? false/c path-string-symbol?
                file-redirection-spec?
                special-redirect?)
                (current-output-port)]
[#:err err (or/c port? false/c path-string-symbol?
                file-redirection-spec?
                special-redirect?)
                (current-error-port)]
[#:strictness strictness (or/c 'strict 'lazy 'permissive) 'lazy]
[#:lazy-timeout lazy-timeout real? 1]
[#:background? background? any/c #f])
any/c]{
Run a pipeline.  Each @racket[member] should be either a @racket[pipeline-member-spec] or a list, where the first of the list is the command and the rest are arguments.  The command may be a symbol, string, path, or function.  If it is a string or path, it will spawn a subprocess.  If it is a function, it will use that function in a thread.  If the command is an @racket[alias-func], it is called with its arguments before the pipeline is started to receive a new command/argument list which replaces it.

A @racket[pipeline-member-spec], in addition to the command/argument list, has an error-port specification.  All lists given will be turned into @racket[pipeline-member-spec]s using the @racket[err] specification.

Each member of the pipeline will have its @racket[current-output-port] connected to the @racket[current-input-port] of the next member.  The first and last members use @racket[in] and @racket[out], respectively, to communicate with the outside world.

All ports specified (@racket[in], @racket[out], @racket[err]) may be either a port, a @racket[file-redirection-spec?], the special redirect value @racket[null-redirect], #f, or a path/string/symbol.
The error port may also be @racket[stdout-redirect], in which case the output port will be used, @racket[stderr-capture-redirect], in which case the error text will be captured and added to the text of any exception, or @racket[shared-stderr-capture-redirect], which is like @racket[stderr-capture-redirect] except that all parts of a pipeline that use it share the port and have their error text in the exception.
If #f is given, then a port will be returned in the pipeline struct returned (similar to @racket[subprocess]).
If a path/string/symbol is given, then a file at that path is opened.

Beware that just as with @racket[subprocess], if you pass #f to get an input, output, or error port out of a pipeline, the resulting port may be a file-stream-port, and you will need to be sure to close it.  Otherwise all file-stream-port handling in the pipeline and for file redirection is done automatically.

@racket[strictness] determines how success is reported.  If @racket[strictness] is @code{'strict}, then the pipeline is successful when all members are successful, and if there are errors the first member to have an error is reported.  If @racket[strictness] is @code{'lazy}, success is similar, but treats any members that were killed as successful.  If @racket[strictness] is @code{'permissive}, then errors are ignored except for the last pipeline member, which is what bash and most other shell languages do.

Also, if @racket[strictness] is @code{'lazy} or @code{'permissive}, then when a pipeline member finishes, pipeline members before it may be killed.  In permissive mode they may be killed immediately, and in lazy mode they have @racket[lazy-timeout] seconds to finish before they are killed.  This process killing happens to not wait for long (potentially infinitely so) processes in the middle of a pipeline when only a small part of their output is used.  For instance, piping the output of a large file (or cat-ing an infinite pseudo-file) to the "head" command.  This mirrors what bash and other shells do.

If @racket[background?] is false, then @racket[run-subprocess-pipeline] uses @racket[pipeline-wait] to wait until it finishes, then returns the status with @racket[pipeline-status].  If @racket[background?] is not false, then @racket[run-subprocess-pipeline] returns a @racket[pipeline] object.

@bold{
The default value for @racket[lazy-timeout] is not stable.
The semantics of the @racket[background?] are not stable.
}
}

@defproc[(run-subprocess-pipeline/out [member (or/c list? pipeline-member-spec?)] ...
[#:in in (or/c input-port? false/c path-string-symbol?) (open-input-string "")]
[#:strictness strictness (or/c 'strict 'lazy 'permissive) 'lazy]
[#:lazy-timeout lazy-timeout real? 1])
any/c]{
Like @racket[run-subprocess-pipeline], but string-ports are used as the input, output, and error ports.  It does not return until the pipeline finishes, and returns the output string.  If the pipeline has an unsuccessful status, an exception is raised (with the contents of the error port).

@bold{
@racket[lazy-timeout]'s default value is not stable.
}
}

@defthing[run-pipeline procedure?]{
Deprecated.

Alias for @racket[run-subprocess-pipeline].
}
@defthing[run-pipeline/out procedure?]{
Deprecated.

Alias for @racket[run-subprocess-pipeline].
}


@defproc[(pipeline-member-spec? [pmspec any/c]) boolean?]{
Is it a pipeline-member-spec?
}
@defproc[(pipeline-member-spec [argl any/c]
[#:err err (or/c port? false/c path-string-symbol?
                 file-redirection-spec?
                 special-redirect?)
                 hidden-default-value]
[#:success success-pred (or/c false/c procedure? (listof any/c)) hidden-default-value])
pipeline-member-spec?]{
Make a pipeline-member-spec.  @racket[argl] is the command/argument list.  The first value in the list is the command, and should either be a @racket[path-string-symbol?] to a command or a function.
@racket[err] is the error port specification for it to use.
@racket[success-pred] is a predicate that will be applied to the return value of a pipeline member to determine its success.  Subprocesses will be considered successful if they return 0 when @racket[success-pred] is #f, or if @racket[success-pred] is a list, if they return 0 or a member of that list, or if @racket[success-pred] is a function, it will be successful if @code{(success-pred return-value)} returns a non-#f value.  Function pipeline members are always considered unsuccessful if they throw an error.  Otherwise, they are successful if @racket[success-pred] is #f, if their return is a member of @racket[success-pred] when it is a list, or when @code{(success-pred return-value)} is true when it is a function.

@racket[err] and @racket[success-pred] default to values that can be overridden by the defaults set by the pipeline-running functions.  But in the end they default to current-error-port and #f.
}

@defproc[(pipeline? [p any/c]) boolean?]{
Is it a pipeline object?  Note that this is not the same as @racket[shell/mixed-pipeline/pipeline?].

Also, pipelines are synchronizable with the @racket[sync] function.
}

@defproc[(pipeline-port-to [p pipeline?]) (or/c false/c output-port?)]{
Get initial input port (if one was provided initially, this will be false)

@bold{This function is not stable.}
}
@defproc[(pipeline-port-from [p pipeline?]) (or/c false/c input-port?)]{
Get final output port (if one was provided initially, this will be false)

@bold{This function is not stable.}
}
@defproc[(pipeline-err-ports [p pipeline?]) (listof (or/c false/c input-port?))]{
Get list of error ports for the pipeline (each one that was provided will be false)

@bold{This function is not stable.}
}
@defproc[(pipeline-wait [p pipeline?]) void?]{
Wait for the pipeline to finish.

@racket[(pipeline-wait pline)] is essentially the same as @racket[(sync pline)].

If the pipeline strictness is permissive, then a pipeline is finished when the ending member of the pipeline is finished.
}
@defproc[(pipeline-kill [p pipeline?]) void?]{
Kill a running pipeline.
}
@defproc[(pipeline-running? [p pipeline?]) boolean?]{
Is the pipeline currently running?
}
@defproc[(pipeline-success? [p pipeline?]) any/c]{
Waits for the pipeline to terminate (according to @racket[pipeline-wait]).  Returns #t if the pipeline was considered successful, else #f.
If the strictness argument is 'strict or 'lazy, then all members must succeed.  If it is 'permissive then only the last one must succeed.
A pipeline member is considered successful if it was a subprocess and returned 0, or if it was a thread and raised no uncaught exceptions.
If strictness is lazy then pipeline members that were killed are also considered successful.
}
@defproc[(pipeline-status [p pipeline?]) any/c]{
Waits for the pipeline to terminate (according to @racket[pipeline-wait]).  Returns the status of the pipeline.
If the strictness was 'strict or 'lazy, then the status will be the status of the first unsuccessful member or the status of the last member.
If the strictness was 'permissive, then the status will be the status of the last member.
The status of any member is its return code for a process, the return of or exception thrown by the function of a thread member, or 'killed if it was killed.
}
@defproc[(pipeline-status/list [p pipeline?]) (listof any/c)]{
A list of the exit statuses of all the pipeline members.

@bold{This function is not stable.}
}

@defproc[(file-redirect
[file path-string-symbol?]
[#:exists exists (or/c 'error 'append 'truncate)])
file-redirection-spec?]{
Create an object that specifies a pipeline to redirect to the given file with the given handling for existing files.
}
@defproc[(file-redirection-spec? [v any/c]) bool/c]{}
@defproc[(special-redirect? [v any/c]) any/c]{}
@defthing[null-redirect special-redirect?]{}
@defthing[stdout-redirect special-redirect?]{}
@defthing[stderr-capture-redirect special-redirect?]{}
@defthing[shared-stderr-capture-redirect special-redirect?]{}


@defproc[(shellify [func procedure?]) procedure?]{
Convenience function for putting Racket functions into pipelines.

Takes a procedure which takes a string as its first argument and returns a string.  Returns a procedure which will turn its @racket[current-input-port] into a string and pass it to the original procedure as its first argument.  It then displays the output string of the function to its @racket[current-output-port].

@bold{This function is not stable.}
}

@defthing[prop:alias-func struct-type-property?]{
Struct property for alias-funcs.  The property should be a function that takes the structure as an argument and produces a function that takes the argument list and produces a new one.

This may be renamed to be less confusing with @racket[shell/pipeline-macro/define-pipeline-alias].

@bold{This is not stable.}
}
@defstruct[alias-func ([func procedure?])]{
Wrapper struct with @racket[prop:alias-func].  An alias function must return a non-empty list suitable for a @racket[pipeline-member-spec].

@bold{If you want an alias in Rash, this is probably not what you want.}  See @racket[shell/pipeline-macro/define-pipeline-alias].

This will likely be renamed to be less confusing.

@bold{This is not stable.}

Examples:
@codeblock|{
;; A simple case -- have an alias that
;; sets initial arguments.
(define ls-alias
  (alias-func (λ args (list* 'ls '--color=auto args))))
;; Slightly more complicated:
;; `find` requires that its path argument go before
;; its modifier flags.
(define find-files-alias
  (alias-func (λ args `(find ,@args -type f))))
}|
}

@defproc[(path-string-symbol?
[p any/c])
boolean?]{
Like @racket[path-string?], except it also includes symbols that would be valid paths.
}

@defform[(and/success e ...)]{
Like @racket[and], but only treats pipeline objects as truthy if they pass @racket[pipeline-success?].

@bold{This is not stable.}
}
@defform[(or/success e ...)]{
Like @racket[or], but only treats pipeline objects as truthy if they pass @racket[pipeline-success?].

@bold{This is not stable.}
}

@subsection{Substitution}

Most shells have various “substitutions”, where an argument to a procedure is replaced.
For example, Bash and friends have “process substitution”, via @tt{<()} and @tt{>()}, which replaces the pipeline in the parentheses with a path to a named pipe or file descriptor for the stdin/stdout of the running pipeline.
Since Rash is fully programmable, users can easily compute whatever they want for any argument -- fully programmable substitutions anywhere!
However, many substitutions, such as substitutions that create a named pipe, it's nice to have automatic cleanup after a pipeline is finished so those temporary resources don't hang around in the file system.

The @racket[shell-substitution] function creates a “substitution” that can have automatic cleanup when the pipeline finishes.
This way, users can substitute references to arbitrary temporary resources that have automatic cleanup.

@defproc[(shell-substitution [thunk (-> hash?)]) shell-substitution?]{
Creates a substitution for subprocess pipeline member specs.
Shell substitutions may be used to pass names to a subprocess that identify procedurally generated things of one kind or another.
For example, Bash and similar shells include @tt{<(some-pipeline)} as a substitution that replaces the @tt{<()} form with the path to a named pipe in the file system that is connected to the output of @tt{some-pipeline}.
This procedure provides user-defined substitutions.

The thunk must return a @racket[hash?] with the following keys:
Required keys:
@itemlist[
@item{@racket['argument] - the thing that will replace the substitution object when running a subprocess pipeline.  This should generally be a string.  If it's another substitution its thunk will be executed recursively.}
]

Optional keys:
@itemlist[
@item{@racket['pipeline-done-procedure] - a procedure of one argument (the pipeline object).  This procedure will be run when the pipeline terminates, and may be used to clean up.  For example, a substitution that creates a temporary file may delete it.}
]

@emph{Not stable.}
I'm not committing to this API yet.

Note: there is actually no guarantee that @racket['pipeline-done-procedure]s will ever run.
In particular, if a program is aborted or crashes before the relevant pipeline finishes, the procedures will not be run.
So don't rely on them for anything security critical.
At the moment with substitutions I've tried, the consequence is that you are left with extra unnecessary named pipes, sockets, etc in a temporary directory.
But don't rely on this for any important property.
}
@defproc[(shell-substitution? [v any/c]) bool/c]{
Predicate for shell substitutions.
}

@subsubsection{Demo Substitutions}
I've written some demo substitutions.
At some point some substitutions should go into the “standard library” of Rash, but as always I don't like commitment.
Let me know if you really want one and we can add useful substitutions to the “standard library”.
But for now these are in the rash-demos package.

They are all @emph{unstable}!

@; TODO - I should separate the documentation into a separate package, then I can just document everything together easily.
These are in @tt{shell/demo/substitutions}
@itemlist[
@item{
@tt{with-output-to-named-pipe-substitution} -- basically a slightly generalized @tt{<()} from bash.
It takes a thunk (that perhaps runs a pipeline), parameterizes the @racket[current-output-port] of the execution to a named pipe.
The name of the pipe is provided to the subprocess.
Example:
@racketblock[
             (require shell/demo/substitutions)
             (run-subprocess-pipeline
              `(cat ,(with-output-to-named-pipe-substitution
                       (λ () (printf "hello\n")))))
             ]
}

@item{
@tt{with-input-from-named-pipe-substitution} -- basically a slightly generalized @tt{>()} from bash.
It takes a thunk (that perhaps runs a pipeline), parameterizes the @racket[current-input-port] of the execution to a named pipe.
The name of the pipe is provided to the subprocess.
This substitution is really not very useful.
Frankly, I've @emph{never} used this (or its bash equivalent @tt{>()}).
Most programs that write out to a file replace that file rather than appending to it, so many places you might think to use it don't even work.
However, some people think a shell can't be complete without it.
So I wrote it.
Example:
@racketblock[
             (require shell/demo/substitutions)
             (run-subprocess-pipeline
              `(echo hello)
              `(tee ,(with-input-from-named-pipe-substitution
                       (λ () (run-subprocess-pipeline '(md5sum -))))))
             ]
}

@item{
@tt{input-port-to-named-pipe-substitution} -- sends the contents of an arbitrary input port to a temporary named pipe, gives that name to the subprocess.
}
@item{
@tt{output-port-to-named-pipe-substitution} -- gives the name of a temporary named pipe to a subprocess, piping the output of that named pipe to the given output port.
}
]

This is in @tt{shell/demo/closure-substitution}:
@itemlist[
@item{@tt{closure-substitution} -- takes an arbitrary Racket procedure, gives to the subprocess the name of a temporary script that it generates.
The substitution creates a thread that listens on a temporary unix domain socket, and the temporary script creates connections to that socket.
The procedure is passed any command line arguments that the script receives, and its execution is @racket[parameterize]d so that @racket[current-input-port] and @racket[current-output-port] are the input and output of the script.

Note that the procedure will only receive input if the optional argument @racket[#:read-stdin?] is true.

Example:
@racketblock[
             (require shell/demo/closure-substitution)
             (define my-proc
               (let ([x 'local-variable])
                 (λ (file-name)
                   (printf "file-name: ~v\n" file-name)
                   (printf "closed over x: ~v\n" x))))
             (run-pipeline
              `(find /bin -type f -exec ,(closure-substitution my-proc) "{}" ";"))
             ]

So now you can pass Racket functions to @tt{find -exec}.
Thanks to Alexis King for suggesting this substitution.
}
]


@subsubsection{Potential Substitutions}

Besides the demo substitutions, you could imagine substitutions that pass a subrocess references to any kind of temporary resource.
(I haven't implemented these, and many of them are of dubious value.  But these are all possible in Rash, unlike your other favorite shell.)

@itemlist[
@item{Temporary file substitution -- pass a process a temporary file to read or write, clean it up when it's done.}
@item{Temporary directory substitution -- pass a process a temporary directory to muck about it, clean it up when it's done.}
@item{File system substitution -- pass a process a reference to a temporary mount point, unmount the file system when the pipeline is finished.  This could actually be useful with various kinds of virtual file systems via FUSE.  Maybe this can make up just a tiny bit for Unix not having good per-process file system namespaces like Plan 9?}
@item{User/group substitution -- for scripts that run as root, pass @tt{sudo} a user/group name/id that's been generated just for the execution of one program, and clean up the user/group after execution.}
@item{URL substitution -- pass a URL that you've just generated to a local web server, stop the web server after the pipeline is done.}
@item{Virtual block file substitution.  You know you want this.}
@item{Symbolic link substitution.  Or maybe hard link substitution.  Or maybe you're on Windows and you want Junction substitution.  }
@item{Host name / IP address substitution.  Send @tt{ssh} the name of a temporary VM you automatically spin up for a single execution.}
]

I don't know when or why you would want most of these.
But the point is you can do this as a user without me extending the core shell language, or even against my wishes for such substitutions to exist.

