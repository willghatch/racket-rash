Note:  The current `master` branch is woefully outdated with respect to the development version.
The current `master` version is essentially something like bash embeddable within racket, and with support for racket functions that act like processes (reading/writing byte streams).
The newer version extends this to make an extensible line-based language that is useful for unix-style byte-stream pipelines (using external programs or racket functions), object pipelines (passing arbitrary racket values), and mixed pipelines.

-------------------

This is a shell language, library, and REPL for Racket.

It is basically a wrapper of my shell-pipeline package.

It is unstable, and core syntax WILL change.

Online documentation is [here](http://docs.racket-lang.org/rash/index.html).
After installing with `raco pkg install rash`, documentation
is available locally by running `raco docs rash`.

To run the repl, either run `racket -l rash/repl` or put your
racket/bin directory on your path and run `rash-repl`.

