This is a shell language, library, and REPL for Racket.

[![asciicast](https://asciinema.org/a/sHiBRIlSM9wHDetDhsVjrCaZi.png)](https://asciinema.org/a/sHiBRIlSM9wHDetDhsVjrCaZi)

Use as a repl that is as convenient for pipelining programs as Bash
is, but has all the power of Racket.  Use as a scripting language with
`#lang rash`.  Embed in normal Racket files with `(require rash)`, and
mix freely with any other Racket language or library.

I gave a talk about Rash at RacketCon 2017.  You can watch it 
[here](https://www.youtube.com/watch?v=WI8uA4KjQJk#t=3h32m40s).
(A higher quality stand-alone video should be available some time in the future.)


Online documentation is
[here](http://docs.racket-lang.org/rash@rash/index.html).  After
installing with `raco pkg install rash`, documentation is available
locally by running `raco docs rash`.  You can install the git version
by running 
`git clone https://github.com/willghatch/racket-rash rash && cd rash && raco pkg install`


Completion
----------

Tab completion and line editing is enabled in the `readline` branch.
It depends on the git version of Racket's `readline-lib` ffi library.
To use it, run `raco pkg upgrade --clone readline-lib` (run it in a
directory where you want to have the readline package git repo) to get
the latest readline package.  Say "yes" to also cloning the `readline`
and `readline-doc` packages, since they are in the same git repo.
Then check out the `readline` branch of rash (`raco pkg upgrade
--clone rash` if you used `raco pkg install rash`) and maybe run `raco
setup`.

Once the next Racket release is made (in or around January 2018), the
readline branch will be merged and be on by default.
