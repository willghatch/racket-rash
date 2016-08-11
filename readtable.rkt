#lang racket/base

(provide line-readtable)

(require scribble/reader)

(define read-newline-symbol '|%%read-newline-symbol|)

(define read-newline
  (case-lambda
    [(ch port)
     read-newline-symbol]
    [(ch port src line col pos)
     (datum->syntax #f read-newline-symbol)
     ]))

(define bare-line-readtable
  (make-readtable #f
                  #\newline 'terminating-macro read-newline
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
  (make-at-readtable #:readtable bare-line-readtable))


(module+ main
  (define (read-seq [port (current-input-port)])
    (let ([out (read port)])
      (if (equal? eof out)
          '()
          (cons out (read-seq port)))))

  (parameterize ([current-readtable line-readtable])
    (read-seq (open-input-string "this . is\n a (test)\nto see @{what happens}")))
  )
