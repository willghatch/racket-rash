#lang racket/base

(provide
 rash-read-syntax
 rash-read-syntax-all
 rash-read
 rash-read-all
 rash-stx-strs->stx
 )

(require
 udelim
 syntax/parse
 syntax/strip-context
 )

(define (rash-read-line-syntax src in)
  (define (rec rlist)
    (let ([output
           (parameterize ([current-readtable line-readtable])
             (read-syntax src in))])
      (cond [(and (eof-object? output) (null? rlist))
             output]
            [(eof-object? output)
             (datum->syntax #f (cons #'%%rash-line-start
                                     (reverse rlist)))]
            [else
             (syntax-parse output
               #:datum-literals (%%rash-newline-symbol)
               [%%rash-newline-symbol
                (if (null? rlist)
                    (rash-read-syntax src in)
                    (datum->syntax output (cons #'%%rash-line-start
                                                (reverse rlist))))]
               [else (rec (cons output rlist))])])))
  (rec '()))

(define (rash-read-syntax src in)
  (read-and-ignore-hspace! in)
  ;; TODO - maybe an extensible table of things to do depending on the start of the line?
  #|
  TODO - if a line starts with a #||# comment then `(` isn't the first
  char, forcing it to be in line-mode and not racket-mode.  That's
  weird.  Looking at the first character is brittle and crappy, but I'm
  not sure a better way to do it right now.
  |#
  (let ([peeked (peek-char in)])
    (cond [(equal? #\( peeked)
           (let ([s (parameterize ([current-readtable rash-inside-paren-readtable])
                      (read-syntax src in))])
             #`(%%rash-racket-line #,s))]
          [(equal? #\; peeked)
           (begin (read-line-comment (read-char in) in)
                  (rash-read-syntax src in))]
          [else (rash-read-line-syntax src in)])))

(define (rash-read in)
  (let ([out (rash-read-syntax #f in)])
    (if (eof-object? out)
        out
        (syntax->datum out))))

(define (rash-read-syntax-all src in)
  (define (rec rlist)
    (let ([part (rash-read-syntax src in)])
      (if (eof-object? part)
          (datum->syntax #f (reverse rlist))
          (rec (cons part rlist)))))
  (rec '()))
(define (rash-read-all in)
  (syntax->datum (rash-read-syntax-all #f in)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ignore-to-newline! port)
  (let ([out (read-char port)])
    (if (or (equal? out #\newline)
            (equal? out eof))
        (void)
        (ignore-to-newline! port))))

(define read-newline
  (case-lambda
    [(ch port)
     (syntax->datum (read-newline ch port #f #f #f #f))]
    [(ch port src line col pos)
     #'%%rash-newline-symbol]))

(define read-line-comment
  (case-lambda
    [(ch port)
     (syntax->datum (read-line-comment ch port #f #f #f #f))]
    [(ch port src line col pos)
     (ignore-to-newline! port)
     #'%%rash-newline-symbol]))

(define (read-and-ignore-hspace! port)
  (let ([nchar (peek-char port)])
    (if (or (equal? #\space nchar)
            (equal? #\tab nchar))
        (begin (read-char port)
               (read-and-ignore-hspace! port))
        (void))))

(define rash-inside-paren-readtable
  (udelimify #f))

(define line-readtable/pre-delim
  (make-readtable #f
                  ;; newline and comment (which ends with newline) need
                  ;; to give newline symbols for parsing
                  #\newline 'terminating-macro read-newline
                  #\; 'terminating-macro read-line-comment

                  ;; take away the special meanings of characters

                  ;; | is seldom used in racket in practice -- who uses symbols
                  ;; that need escaping inside?  But I imagine it will frequently
                  ;; be desired as a pipe identifier.
                  #\| #\a #f

                  ;; . is really only useful in lists (where it will be available),
                  ;; and will want to be literal in a lot of command lines.
                  #\. #\a #f

                  ;; , will be useful in lists also (where it will be available),
                  ;; and will want to be literal in many command lines.
                  #\, #\a #f

                  ;; quote and quasiquote will be useful on the command line, and
                  ;; people are used to them having special meaning and needing to
                  ;; quote them, so they are not commonly required in program argument
                  ;; strings.
                  ;#\` #\a #f
                  ;#\' #\a #f

                  ;; @ doesn't really have any special meaning normally, so we don't
                  ;; need to strip it of any.
                  ;#\@ #\a #f

                  ;; -i is a common flag, but -i is a number constant in racket,
                  ;; so I need to take away the meaning of - in the reader (or
                  ;; make -i flags *really* annoying to use)!
                  ;; TODO - I can use -i if syntax objects carry their literal original
                  ;; string representation inside them, because I could then pull out
                  ;; the original string for these sort of quoted things.
                  #\- #\a #f

                  ;; I want # to have its normal meaning to allow #||# comments,
                  ;; #t and #f, #(vectors, maybe), etc.
                  ;#\# #\a #f
                  ))

(define line-readtable
  (make-list-delim-readtable
   #\[ #\] #:inside-readtable rash-inside-paren-readtable
   #:base-readtable
   (make-list-delim-readtable
    #\{ #\} #:inside-readtable rash-inside-paren-readtable
    #:base-readtable
    (make-list-delim-readtable
     #\( #\) #:inside-readtable rash-inside-paren-readtable
     #:base-readtable
     (make-string-delim-readtable
      #\◸ #\◹ #:wrapper '#%upper-triangles
      #:base-readtable
      (make-string-delim-readtable
       #\◺ #\◿ #:wrapper '#%lower-triangles
       #:base-readtable
       (make-string-delim-readtable
        #\◤ #\◥ #:wrapper '#%full-upper-triangles
        #:base-readtable
        (make-string-delim-readtable
         #\◣ #\◢ #:wrapper '#%full-lower-triangles
         #:base-readtable
         (make-string-delim-readtable #\« #\» #:base-readtable line-readtable/pre-delim)))))))))

(define (rash-stx-strs->stx stx)
  (let ([src (syntax-parse stx
               [(rash-src:str) #'rash-src]
               [(src-seg:str ...+) (scribble-strings->string #'(src-seg ...))])])
    (map (λ (s) (replace-context src s))
         (syntax->list
          (rash-read-syntax-all (syntax-source src)
                                (stx-string->port src))))))
