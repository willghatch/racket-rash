#lang racket/base

(provide
 envar
 tilde-expand
 (for-syntax
  dollar-expand
  has-glob-characters?
  ))

(require
 racket/match
 racket/format
 racket/string
 file/glob
 (for-syntax
  racket/base
  syntax/parse
  racket/match
  racket/string
  ))

(define-syntax (envar stx)
  (define-syntax-class idstr
    (pattern (~or x:id y:str)))
  (syntax-parse stx
    [(rec x:idstr)
     #'(rec x #:default
            (error 'envar
                   "unset environment variable with no default: ~a"
                   (~a 'x)))]
    [(_ x:idstr #:default def:expr)
     #'(or (getenv (~a 'x)) def)]
    [(_ x:idstr ve:expr)
     #'(let ([v ve])
         (and (or (putenv (~a 'x) v)
                  (error 'envar "setting environment variable failed"))
              v))]))

(define (glob-quote str)
  (string-replace
   (string-replace
    (string-replace
     (string-replace str "*" "\\*")
     "?" "\\?")
    "{" "\\{")
   "}" "\\}")
  str)

(define (tilde-expand str)
  (let ([rmatch (regexp-match #px"(~([^/]*)?)(/(.*))?" str)])
    (match rmatch
      [(list whole user+tilde user-no-tilde after+slash after-no-slash)
       (string-append (path->string (expand-user-path user+tilde))
                      (or after-no-slash ""))]
      [else str])))


(define-for-syntax (has-glob-characters? str)
  ;; Detect in a literal string segment whether there are glob characters
  ;; TODO - what is the full list of characters that should induce globbing?
  (regexp-match #px"\\*|\\?|\\{|\\}" str))

;; dollar-expand does Bourne-style expansion of a syntax-string.
;; It produces a form that does all the runtime checks needed and
;; produces a string (or list of strings sometimes under globbing)
(define-for-syntax (dollar-expand str-stx
                                  #:dollar-string [dollar-str "$"]
                                  #:glob-expand? [glob-after? #f]
                                  #:tilde-expand? [tilde-expand-after? #t])
  ;; TODO - get location data better
  (define str (let ([s (syntax->datum str-stx)])
                (or (and (string? s) s)
                    (format "~a" s))))
  (define dollar-regexp-str (regexp-quote dollar-str))
  (define dollar-regexp (pregexp dollar-regexp-str))
  (define dollar-id-end-regexp (pregexp (string-append dollar-regexp-str
                                                       "|\\s|/")))
  (define (dollar-var-str->var-ref str old-stx glob-protect-maybe)
    (with-syntax ([ref (if (equal? str (string-upcase str))
                           #`(envar #,(datum->syntax old-stx str old-stx))
                           (datum->syntax old-stx (string->symbol str) old-stx))])
      (if (and glob-after? glob-protect-maybe)
          #'(glob-quote ref)
          #'ref)))

  (define (dollar-posn->dollar-id-posn posn)
    (match posn
      [(cons start end)
       (match (regexp-match-positions dollar-id-end-regexp str end)
         [(list (cons id-start id-end))
          (cons start (sub1 id-end))]
         [#f (cons start (string-length str))])]))

  ;; This should live somewhere...
  (define (interleave l1 l2)
    (cond [(and (null? l1) (null? l2)) '()]
          [(or (null? l1) (null? l2))
           (error 'interleave "internal error - lists not same length")]
          [else (cons (car l1) (cons (car l2) (interleave (cdr l1) (cdr l2))))]))

  ;; Imperatively set whether we've detected a glob in the literal parts of
  ;; the string so we can decide whether to wrap the result in a glob call.
  (define glob-detected #f)

  (define has-a-dollar-escape? (regexp-match dollar-regexp str))

  (cond
    [has-a-dollar-escape?
     (let* ([dollar-positions (regexp-match-positions* dollar-regexp-str str)]
            [id-posns (for/vector ([d dollar-positions])
                        (dollar-posn->dollar-id-posn d))]
            [literal-parts-but-end
             (for/list ([i (vector-length id-posns)])
               (let* ([prev-end (if (equal? i 0)
                                    0
                                    (cdr (vector-ref id-posns (sub1 i))))]
                      [start (car (vector-ref id-posns i))]
                      [substr (substring str prev-end start)])
                 (when (and glob-after? (has-glob-characters? substr))
                   (set! glob-detected #t))
                 (datum->syntax str-stx substr str-stx)))]
            ;; don't tilde expand if there is a literal tilde from variable expansion
            [has-literal-tilde-start
             (string-prefix? (syntax->datum (car literal-parts-but-end))
                             "~")]
            [last-literal-part
             (let* ([last-end (cdr (vector-ref id-posns
                                               (sub1 (vector-length id-posns))))]
                    [substr (substring str last-end (string-length str))])
               (when (and glob-after? (has-glob-characters? substr))
                 (set! glob-detected #t))
               (datum->syntax str-stx substr str-stx))]
            [dollar-parts
             (for/list ([i (vector-length id-posns)])
               (match (vector-ref id-posns i)
                 [(cons start end)
                  (dollar-var-str->var-ref
                   (substring str (+ (string-length dollar-str) start) end)
                   str-stx
                   (and glob-after? glob-detected))]))]
            [mixed-parts (append (interleave literal-parts-but-end dollar-parts)
                                 (list last-literal-part))])
       (cond
         [(and glob-after? glob-detected)
          #`(glob (string-join (map ~a (list #,@mixed-parts)) ""))]
         [(and tilde-expand-after? has-literal-tilde-start)
          #`(tilde-expand (string-join (map ~a (list #,@mixed-parts)) ""))]
         [else
          #`(string-join (map ~a (list #,@mixed-parts)) "")]))]
    [(and glob-after? (has-glob-characters? str))
     #`(glob #,str-stx)]
    [(and tilde-expand-after? (string-prefix? str "~"))
     #`(tilde-expand #,(datum->syntax str-stx str str-stx))]
    [else (datum->syntax str-stx str str-stx)]))

