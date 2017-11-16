This is a shell language, library, and REPL for Racket.

[![asciicast](https://asciinema.org/a/sHiBRIlSM9wHDetDhsVjrCaZi.png)](https://asciinema.org/a/sHiBRIlSM9wHDetDhsVjrCaZi)

Use as a repl that is as convenient for pipelining programs as Bash
is, but has all the power of Racket.  Use as a scripting language with
`#lang rash`.  Embed in normal Racket files with `(require rash)`, and
mix freely with any other Racket language or library.

I gave a talk about Rash at RacketCon 2017.  You can watch it 
[here](https://www.youtube.com/watch?v=yXcwK3XNU3Y&index=13&list=PLXr4KViVC0qIgkwFFzM-0we_aoOfAl16Y).

Rash is in active development, and is not stable as a language.  But I
use it as my default interactive shell on my laptop.  It's far from
complete, but I already like it much better than Bash.  Give it a try.

Online documentation is
[here](http://docs.racket-lang.org/rash@rash/index.html).  After
installing with `raco pkg install rash`, documentation is available
locally by running `raco docs rash`.  You can install the git version
by running 
`git clone https://github.com/willghatch/racket-rash rash && cd rash && raco pkg install`

See also the [shell-pipeline](https://github.com/willghatch/racket-shell-pipeline)
library that contains most of the actual meat of the Rash language.


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
