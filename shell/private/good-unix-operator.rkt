#lang racket/base

(provide
 define-unixy-pipe
 =default-unix-pipe=
 envar
 )

(require
 racket/string
 racket/match
 racket/format
 file/glob
 "pipeline-operators.rkt"
 (for-syntax
  racket/base
  racket/match
  syntax/parse
  syntax/keyword
  racket/string
  "rash-alias.rkt"
  "filter-keyword-args.rkt"
  "misc-utils.rkt"
  ))


#|
What should the good unix pipe do (or at least have options to do)?

* expansion
** ~ expansion (only with literal tilde, not as the result of variable lookup.  Except)
** $var and $ENV_VAR expansion (the results of the variables should not be expanded as globs [eg. if the variable contains "~" or "*"], but are needed for globbing)
** glob expansion (but the expansions from variables should be protected from being part of the glob...  I may have to implement a custom glob function rather than using file/glob)
* alias support
* non-default option to be an object pipe if the command-position is bound
* non-default option to be an infix math pipe if the command-position is a number (solves the common case of quick calculator math that I use python for)


|#

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

(define(glob-quote str)
  ;; TODO implement right!!!!!
  str)

(define (tilde-expand str)
  (let ([rmatch (regexp-match #px"(~)([^/]+)?(/.*)?" str)])
    (match rmatch
      [(list whole tilde user after)
       (string-append (get-home-dir user) (or after ""))]
      [else str])))
(define (get-home-dir user)
  (if user
      ;; field 6 in /etc/passwd on Linux, but I'm not sure how to get this on MacOS or Windows
      (error 'tilde-expand "Getting the home directory of other users is not yet supported")
      (getenv "HOME")))

(begin-for-syntax
  (define (has-glob-characters? str)
    ;; Detect in a literal string segment whether there are glob characters
    ;; TODO - what is the full list of characters that should induce globbing?
    (regexp-match #px"\\*|\\?|\\{|\\}" str))

  (define (dollar-variable-string-expand str-stx dollar-str
                                         #:glob-expand-after? [glob-after? #f]
                                         #:tilde-expand-after? [tilde-expand-after? #f])
    ;; * get $identifiers with their location in the string
    ;; * split the string into a list of strings and (dollar-expand $something will-glob)
    ;; * put it in a join form, and maybe also a glob form
    ;; TODO - get location data better
    (define str (syntax->datum str-stx))
    (define dollar-regexp-str (regexp-quote dollar-str))
    (define dollar-regexp (pregexp dollar-regexp-str))
    (define dollar-id-end-regexp (pregexp (string-append dollar-regexp-str
                                                         "|\\s|/")))
    (define (dollar-var-str->identifier str old-stx glob-protect-maybe)
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
            [(or (null? l1) (null? l2)) (error 'interleave "lists not same length")]
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
                    (dollar-var-str->identifier
                     (substring str (+ (string-length dollar-str) start) end)
                     str-stx
                     (and glob-after? glob-detected))]))]
              [mixed-parts (append (interleave literal-parts-but-end dollar-parts)
                                   (list last-literal-part))])
         (cond
           [(and glob-after? glob-detected)
            #`(glob (string-join (map ~a (list #,@mixed-parts)) ""))]
           [tilde-expand-after?
            #`(tilde-expand (string-join (map ~a (list #,@mixed-parts)) ""))]
           [else
            #`(string-join (map ~a (list #,@mixed-parts)) "")]))]
      [(and glob-after? (has-glob-characters? str))
       #`(glob #,str-stx)]
      [(and tilde-expand-after? (string-prefix? str "~"))
       #`(tilde-expand #,str-stx)]
      [else str-stx]))


  (define (mk-unix-pipe-tx #:tilde-expand? [tilde-expand? #t]
                           #:dollar-string [dollar-string "$"]
                           #:glob-expand? [glob-expand? #t]
                           #:alias-support? [alias-support? #t]
                           ;; TODO - Maybe options to become an object or infix
                           ;;        math pipe depending on command-position identifier.
                           ;;        But maybe it should be configurable, or just be
                           ;;        a wrapper instead.
                           )
    (define (should-glob-expand? str)
      (and glob-expand? (has-glob-characters? str)))
    (define (should-tilde-expand? str)
      (and tilde-expand? (string-prefix? str "~")))
    (define (should-dollar-expand? str)
      (and dollar-string (regexp-match (regexp-quote dollar-string) str)))

    (define (do-expansions* syntax-str)
      (define str (syntax->datum syntax-str))
      (cond
        [(should-dollar-expand? str)
         (dollar-variable-string-expand syntax-str dollar-string
                                        #:glob-expand-after? glob-expand?
                                        #:tilde-expand-after? tilde-expand?)]
        [(should-glob-expand? str)
         #`(glob #,syntax-str)]
        [(should-tilde-expand? str)
         #`(tilde-expand #,syntax-str)]
        [else #f]))

    (define (do-expansions-maybe stx)
      (syntax-parse stx
        [x:id (or (do-expansions* (datum->syntax stx
                                                 (symbol->string
                                                  (syntax->datum #'x))
                                                 stx))
                  stx)]
        [x:str (or (do-expansions* #'x)
                   stx)]
        [else stx]))

    (define (do-expansion-parse stx)
      (syntax-parse stx
        [(_ arg ...+)
         #`(=quoting-basic-unix-pipe=
            #,@(map do-expansions-maybe (syntax->list #'(arg ...))))]))

    (λ (stx)
      (if alias-support?
          (syntax-parse stx
            [(_ cmd:rash-alias-id arg ...)
             (let ([slv (syntax-local-value #'cmd)])
               ({rash-alias-ref slv} slv #'(cmd arg ...)))]
            [else (do-expansion-parse stx)])
          (do-expansion-parse stx))))
  )

(define-syntax (define-unixy-pipe stx)
  (syntax-parse stx
    [(_ name:id arg ...)
     (define-values (opts rest-stx)
       (parse-keyword-options #'(arg ...)
                              (list (list '#:tilde-expand? check-expression)
                                    (list '#:glob-expand? check-expression)
                                    (list '#:dollar-string check-expression)
                                    (list '#:alias-support? check-expression)
                                    )
                              #:no-duplicates? #t))
     (with-syntax ([tx-name (datum->syntax #'name (gensym 'unix-pipe-tx))])
       #`(begin
           (define-for-syntax tx-name
             (mk-unix-pipe-tx #:tilde-expand? #,(opref opts '#:tilde-expand? #t)
                              #:glob-expand? #,(opref opts '#:glob-expand? #t)
                              #:dollar-string #,(opref opts '#:dollar-string "$")
                              #:alias-support? #,(opref opts '#:alias-support? #t)
                              ))
           (define-pipeline-operator name
             #:start tx-name
             #:joint tx-name)))]))

(define-unixy-pipe =default-unix-pipe=)


