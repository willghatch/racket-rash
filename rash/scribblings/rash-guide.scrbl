#lang scribble/manual

@(require scribble-code-examples)
@(require rash)

@title{RASH Guide}

Rash is a language and library for writing shell scripts and including them in
Racket programs.

Due to sandboxing of the documentation generator, examples will generally show
errors.

To get started, simply @code{(require rash)} in your program, preferable with
@code{#lang at-exp racket/base}.  You can then use @code|{@rash{}}|.

@code-examples[#:lang "at-exp racket" #:context #'rash]|{
                                                         (require rash)
                                                         ;; This will call ls
                                                         ;; The output will go to stdout
                                                         ;; The return value will be the exit status of ls
                                                         @rash{ls}

                                                         ;; This will pipe the output as expected between programs
                                                         ;; The final output will go to stdout
                                                         @rash{ls | grep foobar | wc}

                                                         ;; This will return a the stdout output of the pipeline as a string.
                                                         ;; If the last member of the pipeline exits with a nonzero status, an exception is raised.
                                                         @rash/out{ls | wc -l}

                                                         ;; Newlines separate commands.
                                                         ;; The output for all of them is treated normally.
                                                         ;; Only the exit status of the last line is returned.
                                                         @rash{ls
                                                               whoami
                                                               cowsay "hello there"
                                                               uname}

                                                         ;; You can escape to Racket with @
                                                         ;; The return values of the racket segments should be strings or symbols
                                                         (define my-favorite-flag '-l)
                                                         @rash{ls @my-favorite-flag @(if 'some-test /dev "/etc")}

                                                         ;; Racket functions can be included in a pipeline.
                                                         ;; They should read/write using current-<input/output>-port, and return 0 on success
                                                         ;; To use a racket function, return a closure.
                                                         @rash{@(λ () (printf "Hello~nworld~n")) | grep Hello}

                                                         ;; If you have a function that takes a string and returns a string, you can shellify it.
                                                         ;; shellify turns current-input-port into a string, passes it in, prints the output to current-output-port, and returns 0.
                                                         @rash{ls /etc | @(shellify string-upcase) | grep HOSTNAME}

                                                         ;; If a function in the pipeline raises an exception, the exception is printed to current-error-port and it gives a nonzero exit status
                                                         @rash{@(λ () (error 'something "this is an error"))}

                                                         ;; If the first symbol on the line is & then the line is just run rather than put in a pipeline
                                                         @rash{&@(+ 5 3)}

                                                         ;; So... there you have it.
                                                         (define (my-grep pat)
                                                           (λ () (for ([line (port->lines (current-input-port))])
                                                                   (when (regexp-match pat line)
                                                                     (displayln line)))))
                                                         @rash{@(λ () (printf "hello\nmars\njupiter\nand\nneptune")) | @(shellify string-upcase) | @(my-grep "R")}

                                                         }|

If you use @code{#lang rash}, then the whole module is as if surrounded with @code|{@rash}|.

For more flexibility, but not the fun syntax, use @racket[rash-pipeline] and @racket[rash-pipeline/funcify].


@code-examples[#:lang "at-exp racket" #:context #'rash]|{
                                                         (require rash)

                                                         ;; the arguments are each either a list of symbols/strings or a closure
                                                         ;; rash-pipeline acts like rash
                                                         (rash-pipeline '(ls -l) '(grep foo) (shellify string-upcase) '(wc))
                                                         ;; rash-pipeline/funcify acts like rash/out
                                                         (rash-pipeline/funcify '(uname -a) '(wc -l))
                                                         (rash-pipeline/funcify (λ () (printf "hello")) (shellify string-upcase))

                                                         }|


