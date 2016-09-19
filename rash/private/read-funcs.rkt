#lang racket/base

(provide
 rash-read-syntax*
 rash-read*
 rash-read-syntax
 rash-read
 )

(require udelim)

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


(define (rash-read-syntax-seq src in)
  (let ([result (parameterize ([current-readtable line-readtable])
                  (read-syntax src in))])
    (if (equal? eof result)
        '()
        (cons result (rash-read-syntax-seq src in)))))


(define read-newline
  (case-lambda
    [(ch port)
     '%%rash-newline-symbol]
    [(ch port src line col pos)
     (datum->syntax #f '%%rash-newline-symbol)]))

(define (ignore-to-newline port)
  (let ([out (read-char port)])
    (if (or (equal? out #\newline)
            (equal? out eof))
        '%%rash-newline-symbol
        (ignore-to-newline port))))

(define read-line-comment
  (case-lambda
    [(ch port)
     (ignore-to-newline port)
     '%%rash-newline-symbol]
    [(ch port src line col pos)
     (ignore-to-newline port)
     (datum->syntax #f '%%rash-newline-symbol)]))

(define (mark-dispatched stx)
  (syntax-property stx 'rash-mark-dispatched #t #t))

(define dispatch-read
  (case-lambda
    [(ch port)
     (syntax->datum (dispatch-read ch port #f #f #f #f))]
    [(ch port src line col pos)
     (parameterize ([current-readtable rash-s-exp-readtable])
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

(define rash-s-exp-readtable
  (make-string-delim-readtable #\« #\»))

(define line-readtable
  (make-string-delim-readtable #\« #\» #:base-readtable bare-line-readtable))
