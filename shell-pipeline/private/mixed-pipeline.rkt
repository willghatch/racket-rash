#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [run-mixed-pipeline
   (->* ()
        (#:in (or/c input-port? false/c path-string-symbol?)
         #:out any/c
         #|
         ;; TODO
         ;; This is closer, but it could also be a transformer
         ;; for the output rather than a port-like thing.
         #:out (or/c output-port? false/c path-string-symbol?
         (list/c path-string-symbol?
         (or/c 'error 'append 'truncate)))
         |#
         #:err (or/c output-port?
                     false/c
                     path-string-symbol?
                     file-redirection-spec?
                     (list/c path-string-symbol?
                             (or/c 'error 'append 'truncate)))
         #:strictness (or/c 'strict 'lazy 'permissive)
         #:lazy-timeout real?
         #:bg any/c
         #:return-pipeline-object any/c
         #:object-to-out any/c
         )
        #:rest (listof (or/c unix-pipeline-member-spec?
                             object-pipeline-member-spec?
                             composite-pipeline-member-spec?))
        any/c)]
  [pipeline? (-> any/c boolean?)]
  [pipeline-ends-with-unix-segment? (-> pipeline? boolean?)]
  [pipeline-success? (-> pipeline? any/c)]
  [pipeline-running? (-> pipeline? any/c)]
  [pipeline-return (-> pipeline? any/c)]
  [pipeline-wait (-> pipeline? any/c)]
  [pipeline-kill (-> pipeline? any/c)]
  ;[pipeline-start-ms (-> pipeline? any/c)]
  ;[pipeline-end-ms (-> pipeline? any/c)]
  )

 ;; re-provided from mostly-structs.rkt, already have contracts
 object-pipeline-member-spec
 object-pipeline-member-spec?
 unix-pipeline-member-spec
 unix-pipeline-member-spec?
 composite-pipeline-member-spec
 composite-pipeline-member-spec?
 and/success
 or/success
 )


(require (prefix-in u- "subprocess-pipeline.rkt"))
(require racket/format
         racket/list
         racket/string
         racket/port
         racket/match
         "mostly-structs.rkt"
         "misc-utils.rkt"
         (submod "mostly-structs.rkt" internals)
         )


(struct object-pipeline-member
  (thread ret-box err-box)
  #:transparent)
(define (object-pipeline-member-wait m)
  (thread-wait (object-pipeline-member-thread m)))
(define (object-pipeline-member-success? m)
  (object-pipeline-member-wait m)
  (not (unbox (object-pipeline-member-err-box m))))
(define (object-pipeline-member-kill m)
  (kill-thread (object-pipeline-member-thread m))
  (set-box! (object-pipeline-member-err-box m)
            (with-handlers ([(λ _ #t) (λ (e) e)])
              (error 'run-pipeline "pipeline killed"))))

(define (pipeline-segment-kill seg)
  (if (u-pipeline? seg)
      (u-pipeline-kill seg)
      (object-pipeline-member-kill seg)))
(define (pipeline-segment-wait seg)
  (if (u-pipeline? seg)
      (u-pipeline-wait seg)
      (object-pipeline-member-wait seg)))
(define (pipeline-segment-success? seg)
  (pipeline-segment-wait seg)
  (if (u-pipeline? seg)
      (u-pipeline-success? seg)
      (object-pipeline-member-success? seg)))
(define (pipeline-segment-ret seg)
  (pipeline-segment-wait seg)
  (if (u-pipeline? seg)
      (u-pipeline-status seg)
      (or (unbox (object-pipeline-member-err-box seg))
          (unbox (object-pipeline-member-ret-box seg)))))
(define (pipeline-segment-error seg)
  (pipeline-segment-wait seg)
  (cond [(pipeline-segment-success? seg) #f]
        [(u-pipeline? seg)
         (let ([stderr (u-pipeline-error-captured-stderr seg)]
               [argl (u-pipeline-error-argl seg)]
               [status (u-pipeline-status seg)])
           (with-handlers ([(λ _ #t) (λ (e) e)])
             (if (and stderr (not (equal? stderr "")))
                 (error 'run-pipeline
                        (format
                         "unix pipeline segment ~s terminated with code ~s.  Captured stderr:\n~a\n"
                         argl
                         status
                         stderr))
                 (error 'run-pipeline
                        (format "unix pipeline-segment ~s terminated with code ~s\n"
                                argl status)))))]
        [else (unbox (object-pipeline-member-err-box seg))]))

(struct pipeline
  (manager-thread segment-box start-ms end-ms-box cleaner-thread kill-flag-box)
  #:property prop:evt (λ (pline)
                        (let ([sema (make-semaphore)])
                          (thread (λ ()
                                    (pipeline-wait pline)
                                    (semaphore-post sema)))
                          (wrap-evt sema (λ _ pline)))))

(define (pipeline-running? pl)
  (thread-running? (pipeline-manager-thread pl)))

(define (pipeline-wait/internal pl)
  (thread-wait (pipeline-manager-thread pl))
  (for ([seg (unbox (pipeline-segment-box pl))])
    (pipeline-segment-wait seg)))
(define (pipeline-wait pl)
  (pipeline-wait/internal pl)
  (thread-wait (pipeline-cleaner-thread pl)))
(define (pipeline-success? pl)
  (pipeline-wait pl)
  (for/and ([pm (unbox (pipeline-segment-box pl))])
    (pipeline-segment-success? pm)))
(define (pipeline-return pl)
  (if (pipeline-success? pl)
      (if (pipeline-ends-with-unix-segment? pl)
          (void)
          (pipeline-segment-ret (car (unbox (pipeline-segment-box pl)))))
      (for/or ([pm (reverse (unbox (pipeline-segment-box pl)))])
        (and (not (pipeline-segment-success? pm)) (pipeline-segment-error pm)))))

(define (pipeline-kill pl)
  (set-box! (pipeline-kill-flag-box pl) #t)
  (for ([pm (unbox (pipeline-segment-box pl))])
    (pipeline-segment-kill pm)))

(define (pipeline-ends-with-unix-segment? pl)
  (and (pipeline? pl)
       (u-pipeline? (car (unbox (pipeline-segment-box pl))))))

(define (pipeline-end-ms pl)
  (pipeline-wait pl)
  (unbox (pipeline-end-ms-box pl)))

;; TODO - what APIs should exist for getting intermediate results/statuses from pipelines?
;; They should mirror the shape of the spec -- results of composite members should be the `and` of the results of their sub-parts, and maybe composite members should be able to supply a predicate on the parts to tell if the whole is successful based on the parts.



(define (member-spec? x)
  (or (object-pipeline-member-spec? x)
      (unix-pipeline-member-spec? x)
      (composite-pipeline-member-spec? x)))

(define (flatten-specs specs)
  (define (rec specs flat-specs-rev)
    (cond [(null? specs) flat-specs-rev]
          [(composite-pipeline-member-spec? (car specs))
           (rec (cdr specs)
                (rec (composite-pipeline-member-spec-members (car specs))
                     flat-specs-rev))]
          [else (rec (cdr specs) (cons (car specs) flat-specs-rev))]))
  (reverse (rec specs '())))

(define (->iport arg)
  (if (port? arg)
      arg
      (open-input-string (~a arg))))

(define (pipeline-drive-segment specs arg starter? init-in-port final-out-port
                                default-err strictness lazy-timeout)
  (cond [(unix-pipeline-member-spec? (car specs))
         (drive-unix-segment specs
                             (if starter? init-in-port (->iport arg))
                             final-out-port
                             default-err
                             strictness
                             lazy-timeout)]
        [else (drive-obj-segment specs arg starter?)]))

(define (drive-unix-segment specs in-port final-out-port
                            default-err strictness lazy-timeout)
  ;; TODO - There is probably a better way to handle this error.
  ;;        The things that can go wrong here are alias resolution and
  ;;        not having an executable for the command.
  ;;        Aliases should ideally be resolved before this, and executables
  ;;        checked, but there could always be a race condition if an
  ;;        executable is removed after an initial check is made.
  (with-handlers ([(λ _ #t) (λ (e) (values (object-pipeline-member (thread (λ () (void)))
                                                                   (box #f)
                                                                   (box e))
                                           '()))])
    (define-values (u-specs specs-rest) (splitf-at specs unix-pipeline-member-spec?))
    (define use-out (if (null? specs-rest)
                        final-out-port
                        #f))
    (define sub-pipe-obj (apply u-run-subprocess-pipeline
                                #:in in-port
                                #:out use-out
                                #:err default-err
                                #:strictness strictness
                                #:lazy-timeout lazy-timeout
                                #:background? #t
                                ;; TODO - status options, etc
                                u-specs
                                ))
    (values sub-pipe-obj specs-rest)))

(define (drive-obj-segment specs arg starter?)
  (define rbox (box #f))
  (define ebox (box #f))
  (define driver-thread
    (thread (λ () (with-handlers ([(λ (e) #t) (λ (e) (set-box! ebox e))])
                    (set-box!
                     rbox
                     (if starter?
                         ({object-pipeline-member-spec-func (car specs)})
                         ({object-pipeline-member-spec-func (car specs)} arg)))))))
  (values (object-pipeline-member driver-thread rbox ebox) (cdr specs)))

(define (default-output-transformer p)
  (string-trim (begin0 (port->string p)
                 (close-input-port p))))

(define (run-mixed-pipeline
         #:in [init-in-port (open-input-string "")]
         #:out [final-output-port-or-transformer
                default-output-transformer]
         #:err [default-err 'string-port]
         #:strictness [strictness 'lazy]
         #:lazy-timeout [lazy-timeout 1]
         ;; TODO - make consistent with other run-pipelines
         #:bg [bg #f]
         ;; TODO - better name
         #:return-pipeline-object [return-pipeline-object #f]
         #:object-to-out [object-to-out #f]
         .
         specs)
  (define pline (-run-pipeline specs init-in-port
                               final-output-port-or-transformer
                               default-err strictness lazy-timeout
                               object-to-out))
  (when (not bg) (pipeline-wait pline))
  (if (or bg return-pipeline-object)
      pline
      (let ([ret (pipeline-return pline)]
            [last-seg (car (unbox (pipeline-segment-box pline)))])
        (cond
          [(pipeline-success? pline) ret]
          [else (raise ret)])
        )))

(define (-run-pipeline specs init-in-port final-out-transformer
                       default-err strictness lazy-timeout
                       object-to-out)
  ;; TODO - thread safety - be sure there's not a new segment being created when everything is killed from eg. C-c
  ;; TODO - check all specs before doing anything (IE resolve all aliases, check that all executables exist)
  ;; TODO - arguments for strict/lazy/permissive success, bg, default err-port (including individual string-ports for exceptions), environment extension, environment replacement, etc
  (define seg-box (box '()))
  (define kill-flag-box (box #f))

  (define final-out-port (if (or (output-port? final-out-transformer)
                                 (u-path-string-symbol? final-out-transformer)
                                 (match final-out-transformer
                                   [(list (? u-path-string-symbol?) (? symbol?))
                                    final-out-transformer]
                                   [else #f]))
                             final-out-transformer
                             #f))
  (define out-transform (if final-out-port
                            #f
                            final-out-transformer))

  (define (segment-get-arg s)
    (cond [(u-pipeline? s) (u-pipeline-port-from s)]
          [(object-pipeline-member? s) (unbox (object-pipeline-member-ret-box s))]
          [else #f]))

  (define (drive specs arg starter?)
    (pipeline-drive-segment (flatten-specs specs)
                            arg
                            starter?
                            init-in-port
                            final-out-port
                            default-err
                            strictness
                            lazy-timeout))

  (define end-ms-box (box #f))

  (define (runner-func)
    (define (rec last-seg specs)
      (cond [(unbox kill-flag-box)
             (void)]
            [(and (null? specs)
                  (or (u-pipeline? last-seg)
                      object-to-out)
                  out-transform)
             ;; add implicit transformer pipe segment
             (let-values ([(new-seg specs-rest)
                           (drive (list (object-pipeline-member-spec out-transform))
                                  (segment-get-arg last-seg)
                                  #f)])
               (set-box! seg-box (cons new-seg (unbox seg-box)))
               ;; Done, but make the manager thread wait until the last segment is done.
               (pipeline-segment-wait new-seg))]
            [(and (null? specs)
                  object-to-out
                  final-out-port
                  (object-pipeline-member? last-seg))
             (set! object-to-out #f)
             (rec last-seg (list (object-pipeline-member-spec
                                  (λ (x)
                                    (define port (open-output-spec final-out-port))
                                    (display x port)
                                    (flush-output port)
                                    (when (not (eq? port final-out-port))
                                      (close-output-port port))
                                    (void)))))]
            [(null? specs) (pipeline-segment-wait last-seg)]
            [(object-pipeline-member? last-seg)
             (begin
               (thread-wait (object-pipeline-member-thread last-seg))
               (if (object-pipeline-member-success? last-seg)
                   (let-values ([(new-seg specs-rest)
                                 (drive specs
                                        (segment-get-arg last-seg)
                                        (not last-seg))])
                     (set-box! seg-box (cons new-seg (unbox seg-box)))
                     (rec new-seg specs-rest))
                   ;; Done -- no need to wait since there was an error.
                   (void)))]
            [else
             (let-values ([(new-seg specs-rest)
                           (drive specs
                                  (segment-get-arg last-seg)
                                  (not last-seg))])
               (set-box! seg-box (cons new-seg (unbox seg-box)))
               (rec new-seg specs-rest))]))
    (rec #f specs))

  (define pipeline-almost
    (pipeline (thread runner-func) seg-box
              (current-inexact-milliseconds) end-ms-box
              #f kill-flag-box))

  (define cleaner-thread
    (thread (λ ()
              (pipeline-wait/internal pipeline-almost)
              (set-box! end-ms-box (current-inexact-milliseconds)))))

  (struct-copy pipeline pipeline-almost [cleaner-thread cleaner-thread]))

(define-syntax-rule (and/success e ...)
  (and (let ([tmp e]) (if (pipeline? tmp) (and (pipeline-success? tmp) tmp) tmp)) ...))
(define-syntax-rule (or/success e ...)
  (or (let ([tmp e]) (if (pipeline? tmp) (and (pipeline-success? tmp) tmp) tmp)) ...))

;; TODO - orig. pipelines need <() >() redirects, environment modifiers, ...
