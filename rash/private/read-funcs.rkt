#lang racket/base

(provide
 rash-read-syntax
 rash-read
 rash-read-syntax*
 rash-read*
 )

(require udelim)
(require syntax/parse)

(define (rash-read-syntax src in)
  (parameterize ([current-readtable line-readtable])
    (read-syntax src in)))
(define (rash-read-syntax* src in)
  (define (rec rlist)
    (let ([part (rash-read-syntax src in)])
      (if (eof-object? part)
          (datum->syntax #f (reverse rlist))
          (rec (cons part rlist)))))
  (rec '()))

(define (rash-read in)
  (syntax->datum (rash-read-syntax #f in)))
(define (rash-read* in)
  (syntax->datum (rash-read-syntax* #f in)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ignore-to-newline! port)
  (let ([out (read-char port)])
    (if (or (equal? out #\newline)
            (equal? out eof))
        (void)
        (ignore-to-newline! port))))

(define (read-if-starts-with-paren src port)
  (define (rec rev-stxs)
    (read-and-ignore-hspace port)
    (if (equal? #\( (peek-char port))
        (let ([s (parameterize ([current-readtable rash-dispatch-read-table])
                   (read-syntax src port))])
          (rec (cons s rev-stxs)))
        rev-stxs))
  (reverse (rec '())))

(define (give-newline-or-racket-line-stx src port)
  (let ([line-start-with-paren-stxs (read-if-starts-with-paren src port)])
       (if (null? line-start-with-paren-stxs)
           (datum->syntax #f '%%rash-newline-symbol)
           (datum->syntax #f (list* '%%rash-racket-line line-start-with-paren-stxs)))))

(define read-newline
  (case-lambda
    [(ch port)
     (syntax->datum (read-newline ch port #f #f #f #f))]
    [(ch port src line col pos)
     (give-newline-or-racket-line-stx src port)]))

(define read-line-comment
  (case-lambda
    [(ch port)
     (syntax->datum (read-line-comment ch port #f #f #f #f))]
    [(ch port src line col pos)
     (ignore-to-newline! port)
     (give-newline-or-racket-line-stx src port)]))

(define (read-and-ignore-hspace port)
  (let ([nchar (peek-char port)])
    (if (or (equal? #\space nchar)
            (equal? #\tab nchar))
        (begin (read-char port)
               (read-and-ignore-hspace port))
        (void))))

(define (mark-dispatched stx)
  (syntax-parse stx
    [x:id #'(%%rash-dispatch-marker x)]
    [else stx]))

(define dispatch-read
  (case-lambda
    [(ch port)
     (syntax->datum (dispatch-read ch port #f #f #f #f))]
    [(ch port src line col pos)
     (parameterize ([current-readtable rash-dispatch-read-table])
       (mark-dispatched (read-syntax src port)))]))

(define bare-line-readtable
  (make-readtable #f
                  #\newline 'terminating-macro read-newline
                  #\; 'terminating-macro read-line-comment
                  ;; take away the special meanings of characters
                  #\| #\a #f
                  #\. #\a #f
                  #\( #\a #f
                  #\) #\a #f
                  #\{ #\a #f
                  #\} #\a #f
                  #\[ #\a #f
                  #\] #\a #f
                  #\$ 'terminating-macro dispatch-read
                  ))

(define rash-dispatch-read-table
  (make-string-delim-readtable #\« #\»))

(define line-readtable
  (make-string-delim-readtable #\« #\» #:base-readtable bare-line-readtable))
