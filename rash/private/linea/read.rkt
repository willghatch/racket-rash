#lang racket/base

(provide
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
 )

(require
 udelim
 syntax/parse
 syntax/strip-context
 )

(struct linea-newline-token ())

;; Some current-X definitions that are created here with a bogus initial value
;; so that they can be referenced before their real initial value is created.
(define current-linea-line-readtable (make-parameter #f))
(define current-linea-s-exp-readtable (make-parameter #f))

(define default-linea-line-avoid-list '(#\())
(define current-linea-line-avoid-list (make-parameter default-linea-line-avoid-list))

(define (readtable-or-proc->readtable rtop)
  (cond [(readtable? rtop) rtop]
        [(procedure? rtop) (rtop)]
        [(not rtop) (make-readtable rtop)]
        [else (error 'linea-internal
                     "readtable-or-proc->readtable error -- this shouldn't happen")]))

(define (make-linea-read-funcs
         #:line-readtable [line-readtable current-linea-line-readtable]
         #:s-exp-readtable [s-exp-readtable current-linea-s-exp-readtable]
         #:line-avoid [line-avoid current-linea-line-avoid-list])

  (define (linea-read-syntax src in)
    (read-and-ignore-hspace! in)
    ;; TODO - maybe an extensible table of things to do depending on the start of the line?
    #|
    TODO - if a line starts with a #||# comment then `(` isn't the first
    char, forcing it to be in line-mode and not racket-mode.  That's
    weird.  Looking at the first character is brittle and crappy, but I'm
    not sure a better way to do it right now.
    |#
    (let* ([peeked (peek-char in)]
           [avoid-list (if (procedure? line-avoid)
                           (line-avoid)
                           line-avoid)])
      (cond [(member peeked avoid-list)
             (let ([s (parameterize ([current-readtable (readtable-or-proc->readtable
                                                         s-exp-readtable)])
                        (read-syntax src in))])
               (datum->syntax #f (list '#%linea-not-line s)))]
            [(equal? #\; peeked)
             (begin (read-line-comment (read-char in) in)
                    (linea-read-syntax src in))]
            [else (parameterize ([current-readtable (readtable-or-proc->readtable
                                                     line-readtable)])
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

(define read-dash
  ;; don't read as a number for things like `-i`
  (case-lambda
    [(ch port)
     (syntax->datum (read-dash ch port #f #f #f #f))]
    [(ch port src line col pos)
     (cond
       [(regexp-match-peek #px"^\\d+" port)
        (parameterize ([current-readtable (make-readtable (current-readtable)
                                                          #\- #\- #f)])
          (read-syntax/recursive (object-name port) port ch))]
       [else
        (parameterize ([current-readtable (make-readtable (current-readtable)
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

(define (readtable-add-linea-escape
         l-delim r-delim
         #:base-readtable [base-readtable (current-linea-s-exp-readtable)]
         #:wrapper [wrapper #f]
         #:line-readtable [line-readtable current-linea-line-readtable]
         #:s-exp-readtable [s-exp-readtable current-linea-s-exp-readtable]
         #:line-avoid [line-avoid current-linea-line-avoid-list]
         )

  (define-values (l-read-syntax l-read)
    (make-linea-read-funcs
     #:line-readtable line-readtable
     #:s-exp-readtable s-exp-readtable
     #:line-avoid line-avoid))

  (define wrapper-base
    (syntax-parser
      [(e ...) #`(#,(datum->syntax #f '#%linea-expressions-begin) e ...)]))
  (define (final-wrapper stx)
    (let ([linea-form (wrapper-base stx)])
      (cond [(symbol? wrapper) (datum->syntax #f (list wrapper linea-form))]
            [(procedure? wrapper) (wrapper linea-form)]
            [else linea-form])))

  (make-string-delim-readtable
   l-delim r-delim
   #:base-readtable base-readtable
   #:string-read-syntax l-read-syntax
   #:whole-body-readers? #f
   #:wrapper final-wrapper))

(define default-linea-s-exp-readtable
  (readtable-add-linea-escape
   #\◸ #\◹ #:wrapper '#%upper-triangles
   #:base-readtable
   (readtable-add-linea-escape
    #\◺ #\◿ #:wrapper '#%lower-triangles
    #:base-readtable
    (readtable-add-linea-escape
     #\◤ #\◥ #:wrapper '#%full-upper-triangles
     #:base-readtable
     (readtable-add-linea-escape
      #\◣ #\◢ #:wrapper '#%full-lower-triangles
      #:base-readtable
      (udelimify #f))))))

(define default-linea-line-readtable
  (make-list-delim-readtable
   #\[ #\] #:inside-readtable default-linea-s-exp-readtable
   #:base-readtable
   (make-list-delim-readtable
    #\{ #\} #:inside-readtable default-linea-s-exp-readtable
    #:base-readtable
    (make-list-delim-readtable
     #\( #\) #:inside-readtable default-linea-s-exp-readtable
     #:base-readtable
     (readtable-add-linea-escape
      #\◸ #\◹ #:wrapper '#%upper-triangles
      #:base-readtable
      (readtable-add-linea-escape
       #\◺ #\◿ #:wrapper '#%lower-triangles
       #:base-readtable
       (readtable-add-linea-escape
        #\◤ #\◥ #:wrapper '#%full-upper-triangles
        #:base-readtable
        (readtable-add-linea-escape
         #\◣ #\◢ #:wrapper '#%full-lower-triangles
         #:base-readtable
         (make-string-delim-readtable #\« #\» #:base-readtable line-readtable/pre-delim)))))))))

(define-values (linea-read-syntax linea-read) (make-linea-read-funcs))

(current-linea-s-exp-readtable default-linea-s-exp-readtable)
(current-linea-line-readtable default-linea-line-readtable)
