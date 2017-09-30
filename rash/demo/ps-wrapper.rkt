#lang rash

(provide
 my-ps
 print-table-list-specially
 tablesort
 )

(require
 racket/string
 racket/dict

 text-table
 )

(define (whitespace? c)
  (and (char? c)
       (regexp-match? #px"\\s" (string c))))

(define (eat-white-space in-port)
  (let ([c (peek-char in-port)])
    (when (whitespace? c)
      (read-char in-port)
      (eat-white-space in-port))))

(define (read-word in-port)
  (eat-white-space in-port)
  (let loop ([chars '()])
    (define c (read-char in-port))
    (if (or (eof-object? c)
            (whitespace? c))
        (apply string (reverse chars))
        (loop (cons c chars)))))

(define (interleave l1 l2)
  (cond [(and (null? l1) (null? l2)) '()]
        [(or (null? l1) (null? l2)) (error 'interleave "lists not same length")]
        [else (cons (car l1) (cons (car l2) (interleave (cdr l1) (cdr l2))))]))


(define generic-dict-ref dict-ref)
(struct table-elem
  (alist)
  #:methods gen:dict
  [(define (dict-ref dict key [default (λ () (error "key not found" key))])
     (generic-dict-ref (table-elem-alist dict) key default))])


(define (read-spaced-table in-port
                           #:header-transformer [h-tx #f])
  (define line1 (read-line in-port))
  (define fields-pre-tx (string-split line1))
  (define fields (if h-tx
                     (map h-tx fields-pre-tx)
                     fields-pre-tx))
  (define nfields (length fields))
  (define rows
    (let loop ([lines '()])
      (eat-white-space in-port)
      (if (eof-object? (peek-char in-port))
          (reverse lines)
          (let ([words-but-last
                 (for/list ([i (sub1 nfields)])
                   (read-word in-port))])
            (loop (cons (append words-but-last
                                (list (string-trim (read-line in-port))))
                        lines))))))
  (map (λ (row) (table-elem (map cons fields row)))
       rows))

(define (my-ps)
  (define-values (sproc out in err)
    (subprocess #f #f #f (find-executable-path "ps") "aux"))
  (close-output-port in)
  (define table
    (read-spaced-table out
                       #:header-transformer (λ (h) (string->symbol
                                                    (string-downcase h)))))
  (close-input-port out)
  (close-input-port err)
  table)

#;(let ([tab (my-ps)])
  (for ([header (car tab)])
    (printf "~a " (car header))
    (printf "~n"))
  (for ([row (my-ps)])
    (for ([field row])
      (printf "~a " (cdr field)))
    (printf "~n")
    ))

(define (format-table-list rows)
  (table->string
   #:framed? #f
   #:row-sep? #f
   (cons (map car (table-elem-alist (car rows)))
         (map (λ (elem) (map cdr (table-elem-alist elem))) rows))))

(define ((print-table-list-specially fallback-printer) result)
  (if (and (list? result)
           (not (null? result))
           (andmap table-elem? result))
      (printf "~a~n" (format-table-list result))
      (fallback-printer result)))

(define (tablesort rows #:key [key-sym'pid] #:rev [rev #t])
  (define (getkey elem)
    (let* ([val (dict-ref(table-elem-alist elem) key-sym)]
           [num (string->number val)])
      (if num num val)))
  (define (lt l r)
    (if (string? l)
        string<?
        <))
  (define (gt l r)
    (if (string? l)
        string>?
        >))
  (sort rows (if rev gt lt) #:key getkey))

#;(printf "~a~n" (format-table-list (sort (my-ps) < #:key (λ (x) (string->number
                                                                (dict-ref x 'pid))))))
#;(printf "~a~n" (format-table-list (sort (my-ps) string<? #:key (λ (x) (dict-ref x 'command)))))

#;(let ([tab (my-ps)])
  (printf "~a~n"
          (table->string
           #:framed? #f
           #:row-sep? #f
           (cons (map car (car tab))
                 (map (λ (fields) (map cdr fields)) tab)))))
