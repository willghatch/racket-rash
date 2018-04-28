This is a shell language, library, and REPL for Racket.

Use as a repl that is as convenient for pipelining programs as Bash
is, but has all the power of Racket.  Use as a scripting language with
`#lang rash`.  Embed in normal Racket files with `(require rash)`, and
mix freely with any other Racket language or library.

Rash is in active development, and is not stable as a language.  But I
use it as my default interactive shell on my laptop.  It's far from
complete, but I already like it much better than Bash.  Give it a try.

Online documentation is
[here](http://docs.racket-lang.org/rash@rash/index.html).  After
installing with `raco pkg install rash`, documentation is available
locally by running `raco docs rash`.  You can install the git version
by running 
`git clone https://github.com/willghatch/racket-rash rash && cd rash && raco pkg install`


I gave a talk about Rash at RacketCon 2017.  It's slightly out of date, but you can watch it 
[here](https://www.youtube.com/watch?v=yXcwK3XNU3Y&index=13&list=PLXr4KViVC0qIgkwFFzM-0we_aoOfAl16Y).

And here's a quick demo video (also slightly out of date):
[![asciicast](https://asciinema.org/a/sHiBRIlSM9wHDetDhsVjrCaZi.png)](https://asciinema.org/a/sHiBRIlSM9wHDetDhsVjrCaZi)



This repo also contains the [shell-pipeline](https://github.com/willghatch/racket-shell-pipeline) and linea packages.  They mostly support Rash itself, so they live in the same repo.
