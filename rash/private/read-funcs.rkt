#lang racket/base

(provide
 rash-read-syntax
 rash-read-syntax-all
 rash-read
 rash-read-all
 )

(require udelim)
(require syntax/parse)

#;(define (rash-interactive-read-syntax-line src in)
  (define (rec rlist)
    (let ([output
           (parameterize ([current-readtable line-readtable-interactive])
             (read-syntax src in))])
      (cond [(and (eof-object? output) (null? rlist))
             output]
            [(eof-object? output) (datum->syntax #f (reverse rlist))]
            [else
             (syntax-parse output
               #:datum-literals (%%rash-newline-symbol %%rash-racket-line)
               [%%rash-newline-symbol (datum->syntax output (reverse (cons output rlist)))]
               [(%%rash-racket-line) (datum->syntax output (reverse (cons output rlist)))]
               [else (rec (cons output rlist))])])))
  (rec '()))

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
  ;; TODO - this still fails for #||# comments, and generally seems brittle
  (let ([peeked (peek-char in)])
    (cond [(equal? #\( peeked)
           (let ([s (parameterize ([current-readtable rash-dispatch-read-table])
                      (read-syntax src in))])
             (syntax-parse s
               [(e ...) #'(%%rash-racket-line (e ...))]))]
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

(define (mark-dispatched stx)
  (syntax-parse stx
    [f #'(%%rash-dispatch-marker f)]))
(define (mark-dispatched-splice stx)
  (syntax-parse stx
    [f #'(%%rash-dispatch-marker-splice f)]))

(define dispatch-read
  (case-lambda
    [(ch port)
     (syntax->datum (dispatch-read ch port #f #f #f #f))]
    [(ch port src line col pos)
     (parameterize ([current-readtable rash-dispatch-read-table])
       ;; I need to peek to see if it's actually $$ - this would be
       ;; better multi-character matches could be defined on the readtable!
       (let ([c (peek-char port)])
         (if (equal? c #\$)
             (begin
               (read-char port)
               (mark-dispatched-splice (read-syntax src port)))
             (mark-dispatched (read-syntax src port)))))]))

(define bare-line-readtable
  (make-readtable #f
                  #\newline 'terminating-macro read-newline
                  #\; 'terminating-macro read-line-comment
                  ;; take away the special meanings of characters
                  #\| #\a #f
                  #\. #\a #f
                  #\, #\a #f
                  #\` #\a #f
                  #\' #\a #f
                  #\@ #\a #f
                  ;; -i is a number constant!
                  #\- #\a #f
                  ;#\# #\a #f
                  #\( #\a #f
                  #\) #\a #f
                  #\{ #\a #f
                  #\} #\a #f
                  #\[ #\a #f
                  #\] #\a #f
                  #\$ 'terminating-macro dispatch-read
                  ))
(define newrash-bare-line-readtable
  (make-readtable #f
                  #\newline 'terminating-macro read-newline
                  #\; 'terminating-macro read-line-comment
                  ;; take away the special meanings of characters
                  #\| #\a #f
                  #\. #\a #f
                  #\, #\a #f
                  #\` #\a #f
                  #\' #\a #f
                  #\@ #\a #f
                  ;; -i is a number constant!
                  #\- #\a #f
                  ;#\# #\a #f
                  #\( #\a #f
                  #\) #\a #f
                  #\{ #\a #f
                  #\} #\a #f
                  #\[ #\a #f
                  #\] #\a #f
                  ;#\$ 'terminating-macro dispatch-read
                  ))

(define rash-dispatch-read-table
  (udelimify #f))

(define line-readtable
  (make-string-delim-readtable #\« #\» #:base-readtable bare-line-readtable))
