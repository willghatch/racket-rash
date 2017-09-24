#lang scribble/manual

@(require (for-label racket/base
                     racket/contract
                     shell/pipeline))

@title[#:tag "mixed-pipeline"]{Mixed Unix-style and Racket Object Pipelines}

@defmodule[shell/mixed-pipeline]

@section{shell/mixed-pipeline stability}

This library is not entirely stable.

@section{shell/mixed-pipeline guide}

@section{shell/mixed-pipeline reference}

@; Functions in (current) public interface:
@; run-pipeline
@; TODO - these should not be full struct-out, just the necessary parts in case I want to make them macros to support a fast path for macro pipelines
@; (struct-out obj-pipeline-member-spec)
@; (struct-out composite-pipeline-member-spec)
@; u-pipeline-member-spec
@; pipeline?
@; pipeline-success?
@; pipeline-wait
@; pipeline-ret
@; pipeline-start-ms
@; pipeline-end-ms
@; (rename-out [u-pipeline-default-option pipeline-default-option])
@; apply-output-transformer
@;
@; u-alias-func
