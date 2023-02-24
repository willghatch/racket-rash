#lang scribble/manual

@title[#:tag "shell-pipeline"]{Shell Pipeline Package}
@author+email["William Hatch" "william@hatch.uno"]

The Shell Pipeline package is one of the major components of the @hyperlink["https://docs.racket-lang.org/rash/index.html"]{Rash language}.  But parts of it may be useful for people who want to pipeline subprocesses without all of the Rash language, or without using Rash's line-oriented syntax.

@(table-of-contents)

@include-section["pipeline.scrbl"]
@include-section["mixed-pipeline.scrbl"]
@include-section["pipeline-macro.scrbl"]


@section{Code and License}

The code is available
@hyperlink["https://github.com/willghatch/racket-rash"]{on github}.

This library is licensed under the terms of the MIT license and the
Apache version 2.0 license, at your option.
