#lang racket/base

(provide
 make-linea-read-funcs
 linea-read-syntax
 linea-read
 )

(require
 udelim
 syntax/parse
 syntax/strip-context
 )

(struct linea-newline-token ())

(define (make-linea-read-funcs
         #:line-readtable-mod [line-readtable-mod-func (λ(x)x)]
         #:s-exp-readtable-mod [s-exp-readtable-mod-func (λ(x)x)]
         #:line-avoid [line-avoid '(#\()])

  (define outer-readtable (line-readtable-mod-func line-readtable))
  (define inner-readtable (s-exp-readtable-mod-func linea-inside-paren-readtable))

  (define (linea-read-syntax src in)
    (read-and-ignore-hspace! in)
    ;; TODO - maybe an extensible table of things to do depending on the start of the line?
    #|
    TODO - if a line starts with a #||# comment then `(` isn't the first
    char, forcing it to be in line-mode and not racket-mode.  That's
    weird.  Looking at the first character is brittle and crappy, but I'm
    not sure a better way to do it right now.
    |#
    (let ([peeked (peek-char in)])
      (cond [(member peeked line-avoid)
             (let ([s (parameterize ([current-readtable inner-readtable])
                        (read-syntax src in))])
               (datum->syntax #f (list '#%linea-not-line s)))]
            [(equal? #\; peeked)
             (begin (read-line-comment (read-char in) in)
                    (linea-read-syntax src in))]
            [else (parameterize ([current-readtable outer-readtable])
                    (linea-read-line-syntax src in))])))

  (define (linea-read in)
    (let ([out (linea-read-syntax #f in)])
      (if (eof-object? out)
          out
          (syntax->datum out))))

  (values linea-read-syntax linea-read))

(define (linea-read-line-syntax src in)
  ;; the current-readtable must already be parameterized to the line-readtable
  (define (rec rlist)
    (let ([output (read-syntax src in)])
      (cond [(and (eof-object? output) (null? rlist))
             output]
            [(eof-object? output)
             (datum->syntax #f (cons '#%linea-line
                                     (reverse rlist)))]
            [(linea-newline-token? (syntax-e output))
             (if (null? rlist)
                 (linea-read-syntax src in)
                 (datum->syntax output (cons '#%linea-line
                                             (reverse rlist))))]
            ;; Check if we got a symbol that is equal to just a newline character.
            ;; This happens when the newline is escaped.
            ;; TODO - this is bad.  I really need a multi-character readtable
            ;; key.  If the first character after the newline is not a space, then
            ;; the newline is read as part of the symbol name!
            [(equal? (syntax-e output) '\
                     )
             (rec rlist)]
            [else (rec (cons output rlist))])))
  (rec '()))


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
     (datum->syntax #f (linea-newline-token))]))

(define read-line-comment
  (case-lambda
    [(ch port)
     (syntax->datum (read-line-comment ch port #f #f #f #f))]
    [(ch port src line col pos)
     (ignore-to-newline! port)
     (datum->syntax #f (linea-newline-token))]))

(define (read-and-ignore-hspace! port)
  (let ([nchar (peek-char port)])
    (if (or (equal? #\space nchar)
            (equal? #\tab nchar))
        (begin (read-char port)
               (read-and-ignore-hspace! port))
        (void))))

(define linea-inside-paren-readtable
  (udelimify #f))

(define read-dash
  ;; don't read as a number for things like `-i`
  (case-lambda
    [(ch port)
     (syntax->datum (read-dash ch port #f #f #f #f))]
    [(ch port src line col pos)
     (cond
       [(regexp-match-peek #px"^\\d+" port)
        (parameterize ([current-readtable (make-readtable line-readtable
                                                          #\- #\- #f)])
          (read-syntax/recursive (object-name port) port ch))]
       [else
        (parameterize ([current-readtable (make-readtable line-readtable
                                                          #\- #\a #f)])
          (read-syntax/recursive (object-name port) port ch))])]))

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

                  #\- 'non-terminating-macro read-dash

                  ;; I want # to have its normal meaning to allow #||# comments,
                  ;; #t and #f, #(vectors, maybe), etc.
                  ;#\# #\a #f
                  ))

(define line-readtable
  (make-list-delim-readtable
   #\[ #\] #:inside-readtable linea-inside-paren-readtable
   #:base-readtable
   (make-list-delim-readtable
    #\{ #\} #:inside-readtable linea-inside-paren-readtable
    #:base-readtable
    (make-list-delim-readtable
     #\( #\) #:inside-readtable linea-inside-paren-readtable
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

(define-values (linea-read-syntax linea-read) (make-linea-read-funcs))
