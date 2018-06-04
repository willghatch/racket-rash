#lang scribble/manual

@title[#:tag "linea"]{Linea: line oriented reader}
@author+email["William Hatch" "william@hatch.uno"]

@(require
(for-label
linea/line-macro
linea/line-macro-prop
linea/defaults
linea/read

racket/base
racket/splicing
syntax/parse
))

Linea is a line-oriented reader and one of the main components of the @hyperlink["https://docs.racket-lang.org/rash/index.html"]{Rash language}.  While it was designed for Rash, it is flexible and suited to many potential line-oriented languages.  Linea is simply another way to write s-expressions.@margin-note{Note that you can't represent any arbitrary s-expression with Linea, but you can represent a very useful subset of them.}

@section{Stability}
Not yet stable.  Things may still change a bit.

@section{Linea Guide}
TODO

Explanation of inner/outer reading, readtable modifications, #%symbol defaults.


TL;DR

This module:
@nested[#:style 'code-inset]{
@verbatim{
#lang linea "my-lang-bindings.rkt"

finwe feanor fingolfin finarfin

beren (and) \
  luthien tinuviel

(huan (vs) werewolf-sauron)

manwe orome {
  varda yavanna
  aule (mandos
        nienna #{ulmo tulkas})
  melkor
}
}
}

reads as:
@nested[#:style 'code-inset]{
@verbatim{
(module <some-file-name> "my-lang-bindings.rkt"
  (#%module-begin
    (#%linea-line finwe feanor fingolfin finarfin)
    (#%linea-line beren (and) luthien tinuviel)
    (#%linea-s-exp (huan (vs) werewold-sauron))
    (#%linea-line
     manwe
     orome
     (#%linea-expressions-begin
      (#%linea-line varda yavanna)
      (#%linea-line aule (mandos
                          nienna
                          (#%hash-braces
                           (#%linea-expressions-begin
                            (#%linea-line ulmo tulkas)))))
      (#%linea-line melkor)))))
}
}

@section{Linea Reference}

@subsection{linea/default}
@(declare-exporting linea/default)

TODO:
 #%hash-braces

@defform[(#%linea-expressions-begin e ...)]{
Simply a rename-transformer for @racket[begin].
}
@defform[(#%linea-line starter e ...)]{
If @racket[starter] is a @racket[line-macro], then it acts as #'(starter e ...).  If @racket[starter] is not a @racket[line-macro], then the current default (as set by @racket[with-default-line-macro]) is inserted in place of @racket[#%linea-line].
}
@defform[(#%linea-s-exp e)]{
This is just a pass-through -- @(racket (#%linea-s-exp foo)) simply turns into @(racket foo).
}

@subsection{linea/line-macro}
@(declare-exporting linea/line-macro)

@defform[(define-line-macro name transformer)]{
Defines @racket[name] to be a @racket[line-macro] with @racket[transformer] as its syntax transformer.  Note that identifiers defined by @racket[define-line-macro] can be used both as line-macros and normal macros and behave the same either way.

@codeblock[#:keep-lang-line? #f]{
#lang linea racket/base
;; in a language like Rash that uses the Linea reader...
(require (for-syntax racket/base syntax/parse))
(define-line-macro basic-app
  (syntax-parser [(_ e ...) #'(#%app e ...)]))

basic-app println "hello world"

(define-line-macro my-for
  (syntax-parser
    [(_ i:id (~datum in) from:id ... (~datum do) body:expr)
     #'(for ([i (list 'from ...)])
          body)]))

my-for f in file1.txt file2.txt do {
  basic-app println f
}
}

}

@defform[#:id default-line-macro default-line-macro]{
Syntax parameter used to determine which line macro to place when one is not explicitly given.

Use @racket[with-default-line-macro] to set it for a region of code.
}

@defform[#:kind "line-macro" (with-default-line-macro new-default-line-macro body ...)]{
Executes the bodies with @racket[new-default-line-macro] as the default line-macro.

@codeblock[#:keep-lang-line? #f]{
#lang linea racket/base
(with-default-line-macro basic-app {
  displayln "Oh hi"
  displayln "what's up?"
})

;; or
with-default-line-macro basic-app {
  displayln "Oh hi"
  displayln "what's up?"
}
}
}
@defform[#:kind "line-macro" (splicing-with-default-line-macro new-default-line-macro body ...)]{
Like @racket[with-default-line-macro], only the bodies are spliced into the surrounding context as with @racket[splicing-syntax-parameterize].
}


@subsection{linea/line-macro-prop}
@(declare-exporting linea/line-macro-prop)

@defform[#:kind "syntax class" #:id line-macro line-macro]{
Syntax class for matching line macros.  These are matched by @racket[#%linea-line] to determine whether to insert a default line interpretation.
}

@defthing[prop:line-macro struct-type-property?]{
You can define your own structs that are line macros and maybe other things too with @racket[prop:line-macro].  If you make a struct with this property and it is the @racket[syntax-local-value] of an identifier, then it will match the @racket[line-macro] syntax class.

The property should hold a procedure that takes a struct instance as its first argument and a syntax object as its second argument.

@codeblock[#:keep-lang-line? #f]{
#lang racket/base
(struct my-line-macro-struct
  (transformer)
  #:property prop:line-macro (λ (inst . args)
                               (apply
                                (my-line-macro-struct-transformer inst)
                                args)))
}
}

@defproc[(line-macro? [x any/c]) any/c]{
Detects if @racket[x] is a struct with @racket[prop:line-macro].  You probably don't want to use this directly, use the @racket[line-macro] syntax class.
}



@subsection{#lang linea}
@(declare-exporting linea)

Like @tt{#lang s-exp}, you can use @tt{#lang linea} to read the linea notation.

mylank.rkt:
@codeblock{
#lang racket/base
(require linea/defaults linea/line-macro
         (for-syntax racket/base syntax/parse))

(define-line-macro print-quoted-list
  (syntax-parser
    [(_ e ...) #'(println '(e ...))]))

(provide (all-from-out linea/defaults
                       linea/line-macro
                       racket/base)
         print-quoted-list)
}

use-mylang.rkt
@codeblock{
#lang linea "mylang.rkt"

;; prints '(a b c)
print-quoted-list a b c
with-default-line-macro print-quoted-list {
  ;; prints '(hello world)
  hello world
}

}

@subsection{linea/read}
@(declare-exporting linea/read)
TODO:

 linea-read-syntax

 linea-read

 make-linea-read-funcs

 readtable-add-linea-escape

 default-linea-s-exp-readtable

 default-linea-line-readtable

 default-linea-line-avoid-list

 current-linea-s-exp-readtable

 current-linea-line-readtable

 current-linea-line-avoid-list


@section{Code and License}

The code is available
@hyperlink["https://github.com/willghatch/racket-rash"]{on github}.

This library is licensed under the terms of the LGPL version 3, or (at
your option) any later version published by the Free Software
Foundation (IE LGPL3+).
