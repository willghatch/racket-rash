#lang racket/base

(provide
 define-unix-pipe
 =unix-pipe=
 )

(require
 racket/string
 racket/match
 racket/format
 racket/port
 file/glob
 "pipeline-operators.rkt"
 "../utils/bourne-expansion-utils.rkt"
 "mostly-structs.rkt"
 (for-syntax
  racket/base
  racket/match
  syntax/parse
  syntax/keyword
  racket/string
  "pipeline-alias.rkt"
  "filter-keyword-args.rkt"
  "misc-utils.rkt"
  ))


#|
TODO
What more should the good unix pipe do (or at least have options to do)?
* non-default option to be an object pipe if the command-position is bound
* non-default option to be an infix math pipe if the command-position is a number (solves the common case of quick calculator math that I use python for)
|#


(define (closing-port->string x)
  (begin0 (port->string x) (close-input-port x)))
(define (apply-output-transformer transformer out-port)
  (match transformer
    ['port out-port]
    ['string (closing-port->string out-port)]
    ['trim (string-trim (closing-port->string out-port))]
    ['lines (string-split (closing-port->string out-port) "\n")]
    ['words (string-split (closing-port->string out-port))]
    [tx (if (procedure? tx)
            ;; TODO - if this doesn't read the whole port there could be problems
            (tx out-port)
            (error 'apply-output-transformer
                   (format "Neither a procedure nor a known transformer name: ~a"
                           tx)))]))


(define-for-syntax (mk-unix-pipe-tx
                    #:tilde-expand? [tilde-expand? #t]
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
       (dollar-expand-syntax syntax-str
                             #:dollar-string dollar-string
                             #:glob-expand? glob-expand?
                             #:tilde-expand? tilde-expand?)]
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
      [(_ arg-maybe-opt ...+)
       (define-values (kwargs pargs) (filter-keyword-args #'(arg-maybe-opt ...)))
       (define opts
         (parse-keyword-options/eol (datum->syntax #f kwargs)
                                    (list
                                     (list '#:as check-expression)
                                     (list '#:e> check-expression)
                                     (list '#:e>! check-expression)
                                     (list '#:e>> check-expression)
                                     (list '#:err check-expression)
                                     ;(list '#:env check-expression)
                                     (list '#:success check-expression)
                                     )
                                    #:incompatible '((#:e> #:e>! #:e>> #:err))
                                    #:no-duplicates? #t))
       (define err
         (cond [(opref opts '#:e> #f)
                => (λ (out) #`(file-redirect
                               #,(do-expansions-maybe out)
                               #:exists 'error))]
               [(opref opts '#:e>> #f)
                => (λ (out) #`(file-redirect
                               #,(do-expansions-maybe out)
                               #:exists 'append))]
               [(opref opts '#:e>! #f)
                => (λ (out) #`(file-redirect
                               #,(do-expansions-maybe out)
                               #:exists 'truncate))]
               [(opref opts '#:err #f)]
               [else #'(pipeline-default-option)]))
       (define as-stx (opref opts '#:as #f))
       (define quoted-expanded-args
         (map (syntax-parser
                [x:id #'(quote x)]
                [e #'e])
              (map do-expansions-maybe pargs)))
       (define unix-pipe-stx
         #`(=basic-unix-pipe=
            #:err #,err
            #:success #,(opref opts '#:success #'(pipeline-default-option))
            #,@quoted-expanded-args))
       (if as-stx
           #`(=composite-pipe= #,unix-pipe-stx
                               (\|> apply-output-transformer #,as-stx))
           unix-pipe-stx)]))

  (λ (stx)
    (if alias-support?
        (syntax-parse stx
          [(_ cmd:pipeline-alias-id arg ...)
           (let ([slv (syntax-local-value #'cmd)])
             ({pipeline-alias-ref slv} slv #'(cmd arg ...)))]
          [else (do-expansion-parse stx)])
        (do-expansion-parse stx))))



(define-syntax (define-unix-pipe stx)
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

(define-unix-pipe =unix-pipe=)


