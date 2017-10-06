#lang scribble/manual

@title[#:tag "linea"]{Linea: line-based, interaction-friendly syntax}
@author+email["William Hatch" "william@hatch.uno"]

@defmodule[linea]

@section{Stability}

Linea was created for Rash, and I'm not 100% sure right now whether I will fold it back into rash (and get rid of the name Linea entirely), or split it into a separate package.  One of those two things will happen some time in the near-ish future.  Probably it will be folded back in.

@section{Linea Guide}

TODO - show some examples, explain it...

@section{Linea Reference}

things to document:

 linea-read-syntax
 linea-read-syntax-all
 linea-read
 linea-read-all
 linea-stx-strs->stx

 linea-line-parse

 do-line-macro
 default-line-macro
 define-line-macro

 prop:line-macro
 line-macro?

 how to change the inside/outside readtable (not yet implemented)





@section{Code and License}

The code is available
@hyperlink["https://github.com/willghatch/racket-rash"]{on github}.

This library is licensed under the terms of the LGPL version 3, or (at
your option) any later version published by the Free Software
Foundation (IE LGPL3+).
