#lang scribble/manual

@title[#:tag "rash"]{RASH: RAcket SHell Library}
@author+email["William Hatch" "william@hatch.uno"]

@defmodule[rash]
@(require rash)

@bold{Rash}, @italic{adj} 1.  Hurrying into action or assertion without due caution and regardless of prudence, hasty, reckless, precipitate.  “@italic{A rash programmer is likely to produce shell scripts.}”

@section{Stability}

Rash is not entirely stable.  It is not quite ready for a stable release.  But it is definitely ready to have fun with!

@section{RASH Guide}

TODO

@section{RASH Reference}

Also be sure to see
@secref["pipeline-macro"
        #:doc '(lib "shell/scribblings/shell-pipeline.scrbl")]
for documentation on running pipelines, defining and using pipeline operators, etc.

things to document:

rash

rash/wired

mk-rash-macro

mk-rash-module-begin

pipeline-line-macro

cd



from linea:

define-line-macro

do-line-macro

default-line-macro

docs about reader

how to change the inside/outside readtable

link to docs about pipeline macros


@section{Interactive Use}

TODO

how to refer to old repl results

how to change the default pipe in the repl

rashrc stuff



@section{Code and License}

The code is available
@hyperlink["https://github.com/willghatch/racket-rash"]{on github}.

This library is licensed under the terms of the LGPL version 3, or (at
your option) any later version published by the Free Software
Foundation (IE LGPL3+).
