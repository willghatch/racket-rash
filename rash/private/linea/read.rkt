#lang racket/base

(provide
 linea-read-syntax
 linea-read-syntax-all
 linea-read
 linea-read-all
 linea-stx-strs->stx
 )

(require
 udelim
 syntax/parse
 syntax/strip-context
 )

(define (linea-read-line-syntax src in)
  (define (rec rlist)
    (let ([output
           (parameterize ([current-readtable line-readtable])
             (read-syntax src in))])
      (cond [(and (eof-object? output) (null? rlist))
             output]
            [(eof-object? output)
             (datum->syntax #f (cons #'%%linea-line-start
                                     (reverse rlist)))]
            [else
             (syntax-parse output
               #:datum-literals (%%linea-newline-symbol)
               [%%linea-newline-symbol
                (if (null? rlist)
                    (linea-read-syntax src in)
                    (datum->syntax output (cons #'%%linea-line-start
                                                (reverse rlist))))]
               [else (rec (cons output rlist))])])))
  (rec '()))

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
    (cond [(equal? #\( peeked)
           (let ([s (parameterize ([current-readtable linea-inside-paren-readtable])
                      (read-syntax src in))])
             #`(%%linea-racket-line #,s))]
          [(equal? #\; peeked)
           (begin (read-line-comment (read-char in) in)
                  (linea-read-syntax src in))]
          [else (linea-read-line-syntax src in)])))

(define (linea-read in)
  (let ([out (linea-read-syntax #f in)])
    (if (eof-object? out)
        out
        (syntax->datum out))))

(define (linea-read-syntax-all src in)
  (define (rec rlist)
    (let ([part (linea-read-syntax src in)])
      (if (eof-object? part)
          (datum->syntax #f (reverse rlist))
          (rec (cons part rlist)))))
  (rec '()))
(define (linea-read-all in)
  (syntax->datum (linea-read-syntax-all #f in)))


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
     #'%%linea-newline-symbol]))

(define read-line-comment
  (case-lambda
    [(ch port)
     (syntax->datum (read-line-comment ch port #f #f #f #f))]
    [(ch port src line col pos)
     (ignore-to-newline! port)
     #'%%linea-newline-symbol]))

(define read-backslash
  ;; TODO - this wants to be a two-character string pattern in a readtable, rather than a 1-character pattern.
  (case-lambda
    [(ch port)
     (syntax->datum (read-backslash ch port #f #f #f #f))]
    [(ch port src line col pos)
     (let ([next (peek-char port)])
       (if (equal? #\newline next)
           (begin (read-char)
                  (read-syntax src port))
           (let ([rt (make-readtable line-readtable
                                     #\\ #\\ #f)])
             ;; read with normal backslash handling
             (read-syntax/recursive src port ch rt #f))))]))

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

                  #\\ 'non-terminating-macro read-backslash
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

(define (linea-stx-strs->stx stx)
  (let ([src (syntax-parse stx
               [(linea-src:str) #'linea-src]
               [(src-seg:str ...+) (scribble-strings->string #'(src-seg ...))])])
    (map (λ (s) (replace-context src s))
         (syntax->list
          (linea-read-syntax-all (syntax-source src)
                                 (stx-string->port src))))))
