#lang racket/base

(provide
 rash-read-syntax
 rash-read
 rash-parse-at-reader-output
 )

(require (prefix-in scribble: scribble/reader))

(define (rash-read-syntax src in)
  (let ([at-output (scribble:read-syntax-inside src in)])
    (rash-parse-at-reader-output at-output #:src src)))

(define (rash-read in)
  (syntax->datum (rash-read-syntax #f in)))

(define (rash-parse-at-reader-output argl
                                #:src [src #f])
  (for/fold ([out-list '()])
            ([str-or-atout (if (syntax? argl)
                               (syntax->list argl)
                               argl)])
    (let ([datum (if (syntax? str-or-atout)
                     (syntax->datum str-or-atout)
                     str-or-atout)])
      (if (string? datum)
          (append
           out-list
           (map mark-for-quoting
                (rash-read-syntax-seq
                 src (open-input-string datum))))
          (append out-list (list str-or-atout))))))

(define (mark-for-quoting stx)
  (syntax-property stx 'rash-mark-for-quoting #t #t))


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
                  ))

(define line-readtable
  (scribble:make-at-readtable #:readtable bare-line-readtable))
