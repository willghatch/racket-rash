#lang scribble/manual

@title{RASH api}

@(require (for-label rash
                     racket/base
                     racket/contract))

@defmodule[rash]

THIS IS ALL ALPHA AND UNSTABLE


@defproc[(shellify [f (-> string? string?)]) procedure?]{
Wraps a procedure to make it shell pipeline friendly.  The output
procedure will take 0 arguments, will read its input string from
@racket[current-input-port], and will display its output string on
@racket[current-output-port].  It will return 0.
}


@section{TODO}

these are the functions I need to document here:

 shellify
 rash-pipeline
 rash-pipeline/funcify
 pipeline-wait
 pipeline-kill
 pipeline-status
 pipeline-status/all

 rash-line-parse
 rash-line
 rash
 rash/out
 rash/values