<p align="center"><img src="./img/rash-logo.svg" width="200"></p>
<h1 align="center">Rash: The Reckless Racket Shell</h1>

<p align="center">
<a href="https://travis-ci.org/willghatch/racket-rash"><img src="https://travis-ci.org/willghatch/racket-rash.svg?branch=master"></a>
<a href="http://docs.racket-lang.org/rash@rash/index.html"><img src="https://img.shields.io/badge/Docs-Scribble-blue.svg" alt="Scribble Docs"></a>
</p>


Rash is a shell language, library, and REPL for Racket.

Use as a repl that is as convenient for pipelining programs as Bash is, but has all the power of Racket.  Use as a scripting language with `#lang rash`.  Embed in normal Racket files with `(require rash)`, and mix freely with any other Racket language or library.

Rash is in active development, but it is largely stable (and the parts that aren't are marked as such).  I use it as my default interactive shell on my laptop.  It currently lacks the interactive polish of Zsh or Fish, but it is *so* much better as a language.  Every script I've ported from a bourne-related shell to Rash is more robust, simpler, easier to maintain, easier to expand, and much more fun.


## Getting started

### Prerequisites

Rash does work on windows, but it works better and is more useful on unix based systems.

To install, you will need a working instalation of [racket](https://download.racket-lang.org/) v6.12 or later.

### Installation
You can either install with racket's built in package manager, [`raco`](https://docs.racket-lang.org/raco/), or install directly from github.  If you have DrRacket installed, you can install rash with `File -> Install Package`.
#### via raco:
`raco pkg install rash`

#### git version:
`git clone https://github.com/willghatch/racket-rash rash && cd rash/linea && raco pkg install && cd ../shell-pipeline && raco pkg install && cd ../rash && raco pkg install`

OR

use `raco pkg install --clone rash`

#### Readline

The Rash REPL currently relies on Racket's Readline FFI wrapper.  However, by default Racket uses libedit instead of libreadline for licensing reasons.  Libedit does not support unicode, so typing non-ASCII characters will result in sadness.  To use libreadline instead, run `raco pkg install readline-gpl`. Note: `readline-gpl` may need `libreadline-dev` in turn. See this [issue](https://github.com/racket/readline-gpl/issues/3). (On Linux-Debian distributions you can install by `sudo apt install --yes libreadline-dev`.)


### Usage

Run with `racket -l rash/repl --`, or with `rash-repl` if you have Racket's package `bin/` directory on your path.

Online documentation is [here](http://docs.racket-lang.org/rash@rash/index.html).  After installation, local documentation can be accessed with `raco docs rash`.

The project web site has a [quick demo video of Rash in action](https://rash-lang.org).

I published [a paper about Rash in GPCE 2018](http://willghatch.net/publications/rash-gpce-2018-preprint.pdf).

This repo also contains the [shell-pipeline](https://docs.racket-lang.org/shell-pipeline/index.html) and [linea](http://docs.racket-lang.org/linea/index.html) packages.  They mostly support Rash itself, so they live in the same repo.

### Talk

Feel free to ask questions in issues, to join the [matrix room](https://matrix.to/#/#rash-lang:matrix.org), to email me, etc.
