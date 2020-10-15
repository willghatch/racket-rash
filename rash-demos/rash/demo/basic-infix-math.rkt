#lang rash

(provide infix-math)

(require
 (for-syntax
  racket/base
  syntax/parse
  racket/match
  racket/string
  ))

(begin-for-syntax
  (define (oper->precidence op)
    (syntax-parse op
      #:datum-literals (+ - / * ^)
      [+ 1]
      [- 1]
      [/ 2]
      [* 2]
      [^ 3]))

  (define (op? stx)
    (syntax-parse stx
      #:datum-literals (+ - / * ^)
      [(~or + - / * ^) #t]
      [else #f]))

  (define (shunt rpn-out operator-stack inputs)
    (match inputs
      [(list) (rpn->s-exprs (append (reverse operator-stack) rpn-out))]
      [(list (? op? o) in-rest ...)
       (match operator-stack
         [(list) (shunt rpn-out (list o) in-rest)]
         [(list top-op ops-rest ...)
          (if (or (< (oper->precidence o) (oper->precidence top-op))
                  (and (= (oper->precidence o) (oper->precidence top-op))
                       (not (eq? (syntax->datum top-op) '^))))
              (shunt (cons top-op rpn-out) ops-rest inputs)
              (shunt rpn-out (cons o operator-stack) in-rest))])]
      [(list not-op in-rest ...)
       (define rec-form
         (syntax-parse not-op
           [(arg1 arg ...)
            ;; Recur with infix-math if paren-shape is #f
            ;; and there is no #% identifier inside...
            ;; Bad heuristic, but good enough for a demo.
            (if (and (equal? (syntax-property not-op 'paren-shape)
                             #f)
                     (not (and (identifier? #'arg1)
                               (string-prefix?
                                (symbol->string (syntax->datum #'arg1))
                                "#%"))))
                #'(infix-math arg1 arg ...)
                not-op)]
           [else not-op]))
       (shunt (cons rec-form rpn-out) operator-stack in-rest)]))

  (define (rpn->s-exprs rpn-list)
    (define done-stack
      (for/fold ([stack '()])
                ([x (reverse rpn-list)])
        (cond [(op? x)
               (match stack
                 [(list s1 s2 s-rest ...)
                  (cons #`(#,x #,s2 #,s1) s-rest)]
                 [_ (raise-syntax-error
                     'infix-math
                     "operator didn't have enough operands"
                     x)])]
              [else (cons x stack)])))
    (match done-stack
      [(list x) x]
      [(list x y r ...)
       (raise-syntax-error 'infix-math "too many operands in infix expression" y)]))

  )

(define-syntax infix-math
  (syntax-parser
    [(_ arg ...+)
     (shunt '() '() (syntax->list #'(arg ...)))]))


(module+ test
  (require rackunit)
  (check-equal? (infix-math 6 + 7 * 3)
                27)
  (define ^ expt)
  (check-equal? (infix-math 6 ^ 2 + 2)
                38)
  (check-equal? (infix-math 3 + 2 ^ 3 ^ 4)
                (+ 3 (expt 2 (expt 3 4))))
  (check-equal? (infix-math 3 + 2 ^ 3 ^ 4 / 27 - 13 * 12 + 2 * -3)
                (+ (- (+ 3 (/ (expt 2 (expt 3 4)) 27))
                      (* 13 12))
                   (* 2 -3)))

  (check-equal? (infix-math (3 + 2) * 5)
                25)
  (check-equal? (infix-math [+ 3 2] * 5)
                25)
  (check-equal? (infix-math #{|> + 3 5 |> - 7} * 5)
                (* 5 (- 7 (+ 3 5))))
  )
