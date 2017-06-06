#lang racket/base

(provide
 run-pipeline
 (struct-out obj-pipeline-member-spec)
 (struct-out composite-pipeline-member-spec)
 u-pipeline-member-spec
 pipeline?
 pipeline-success?
 pipeline-wait
 pipeline-ret
 pipeline-start-ms
 pipeline-end-ms
 )


(require (prefix-in u- "pipeline.rkt"))
(require racket/format
         racket/list
         racket/string
         racket/port
         racket/match
         )

(struct obj-pipeline-member-spec
  (func)
  #:transparent)
(struct composite-pipeline-member-spec
  (members)
  #:transparent)

(struct obj-pipeline-member
  (thread ret-box err-box)
  #:transparent)
(define (obj-pipeline-member-wait m)
  (thread-wait (obj-pipeline-member-thread m)))
(define (obj-pipeline-member-success? m)
  (obj-pipeline-member-wait m)
  (not (unbox (obj-pipeline-member-err-box m))))

(define (pipeline-segment-wait seg)
  (if (u-pipeline? seg)
      (u-pipeline-wait seg)
      (obj-pipeline-member-wait seg)))
(define (pipeline-segment-success? seg)
  (pipeline-segment-wait seg)
  (if (u-pipeline? seg)
      (u-pipeline-success? seg)
      (obj-pipeline-member-success? seg)))
(define (pipeline-segment-ret seg)
  (pipeline-segment-wait seg)
  (if (u-pipeline? seg)
      (u-pipeline-status seg)
      (or (unbox (obj-pipeline-member-err-box seg))
          (unbox (obj-pipeline-member-ret-box seg)))))

(struct pipeline
  (manager-thread segment-box start-ms end-ms-box cleaner-thread)
  #:property prop:evt (λ (pline)
                        (let ([sema (make-semaphore)])
                          (thread (λ ()
                                    (pipeline-wait pline)
                                    (semaphore-post sema)))
                          (wrap-evt sema (λ _ pline)))))

(define (pipeline-wait/internal pl)
  (thread-wait (pipeline-manager-thread pl))
  (for ([seg (unbox (pipeline-segment-box pl))])
    (pipeline-segment-wait seg)))
(define (pipeline-wait pl)
  (pipeline-wait/internal pl)
  (thread-wait (pipeline-cleaner-thread pl)))
(define (pipeline-success? pl)
  ;; TODO - fix
  (pipeline-segment-success? (car (unbox (pipeline-segment-box pl)))))
(define (pipeline-ret pl)
  (pipeline-segment-ret (car (unbox (pipeline-segment-box pl)))))
(define (pipeline-ends-with-unix-seg? pl)
  (u-pipeline? (car (unbox (pipeline-segment-box pl)))))

(define (pipeline-end-ms pl)
  (pipeline-wait pl)
  (unbox (pipeline-end-ms-box pl)))

;; TODO - what APIs should exist for getting intermediate results/statuses from pipelines?
;; They should mirror the shape of the spec -- results of composite members should be the `and` of the results of their sub-parts, and maybe composite members should be able to supply a predicate on the parts to tell if the whole is successful based on the parts.



(define (member-spec? x)
  (or (obj-pipeline-member-spec? x)
      (u-pipeline-member-spec? x)
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
  (cond [(u-pipeline-member-spec? (car specs))
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
  (with-handlers ([(λ _ #t) (λ (e) (values (obj-pipeline-member (thread (λ () (void)))
                                                                (box #f)
                                                                (box e))
                                           '()))])
    (define-values (u-specs specs-rest) (splitf-at specs u-pipeline-member-spec?))
    (define use-out (if (null? specs-rest)
                        final-out-port
                        #f))
    (define sub-pipe-obj (apply u-run-pipeline
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
                         ({obj-pipeline-member-spec-func (car specs)})
                         ({obj-pipeline-member-spec-func (car specs)} arg)))))))
  (values (obj-pipeline-member driver-thread rbox ebox) (cdr specs)))

(define (default-output-transformer p)
  (string-trim (port->string p)))

(define (run-pipeline specs
                      #:in [init-in-port (open-input-string "")]
                      #:out [final-output-port-or-transformer
                             default-output-transformer]
                      #:err [default-err 'string-port]
                      #:strictness [strictness 'lazy]
                      #:lazy-timeout [lazy-timeout 1]
                      #:bg [bg #f]
                      ;; TODO - better name
                      #:return-pipeline-object [return-pipeline-object #f])
  (define pline (-run-pipeline specs init-in-port final-output-port-or-transformer default-err strictness lazy-timeout))
  (when (not bg) (pipeline-wait pline))
  (if (or bg return-pipeline-object)
      pline
      (let ([ret (pipeline-ret pline)]
            [last-seg (car (unbox (pipeline-segment-box pline)))])
        ;; TODO - get the error correctly for different kinds of pipelines, including error text
        (cond
          [(pipeline-success? pline) ret]
          [(exn? ret) (raise ret)]
          [(pipeline-ends-with-unix-seg? pline)
           (let ([stderr (u-pipeline-error-captured-stderr last-seg)]
                 [argl (u-pipeline-error-argl last-seg)])
             (if (and stderr (not (equal? stderr "")))
                 (error 'run-pipeline
                        (format
                         "unix pipeline segment ~a terminated with code ~a.  Captured stderr:~n~a~n"
                         argl
                         ret
                         stderr))
                 (error 'run-pipeline
                        (format "unix pipeline-segment ~a terminated with code ~a~n"
                                argl ret))))]
          [else (error 'run-pipeline "pipeline error - TODO - give real info")])
        )))

(define (-run-pipeline specs init-in-port final-out-transformer
                       default-err strictness lazy-timeout)
  ;; TODO - thread safety - be sure there's not a new segment being created when everything is killed from eg. C-c
  ;; TODO - check all specs before doing anything (IE resolve all aliases, check that all executables exist)
  ;; TODO - arguments for strict/lazy/permissive success, bg, default err-port (including individual string-ports for exceptions), environment extension, environment replacement, etc
  (define seg-box (box '()))

  (define final-out-port (if (or (output-port? final-out-transformer)
                                 (u-path-string-symbol? final-out-transformer)
                                 (match final-out-transformer
                                   [(list (? u-path-string-symbol?) (? symbol?)) #t]
                                   [else #f]))
                             final-out-transformer
                             #f))
  (define out-transform (if final-out-port
                            #f
                            final-out-transformer))

  (define (segment-get-arg s)
    (cond [(u-pipeline? s) (u-pipeline-port-from s)]
          [(obj-pipeline-member? s) (unbox (obj-pipeline-member-ret-box s))]
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
      (cond [(and (null? specs) (u-pipeline? last-seg) out-transform)
             ;; add implicit transformer pipe segment
             (let-values ([(new-seg specs-rest)
                           (drive (list (obj-pipeline-member-spec out-transform))
                                  (segment-get-arg last-seg)
                                  #f)])
               (set-box! seg-box (cons new-seg (unbox seg-box)))
               ;; Done.
               (void))]
            [(null? specs) (void)]
            [(obj-pipeline-member? last-seg)
             (begin
               (thread-wait (obj-pipeline-member-thread last-seg))
               (if (obj-pipeline-member-success? last-seg)
                   (let-values ([(new-seg specs-rest)
                                 (drive specs
                                        (segment-get-arg last-seg)
                                        (not last-seg))])
                     (set-box! seg-box (cons new-seg (unbox seg-box)))
                     (rec new-seg specs-rest))
                   ;; Done.
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
              #f))

  (define cleaner-thread
    (thread (λ ()
              (pipeline-wait/internal pipeline-almost)
              (set-box! end-ms-box (current-inexact-milliseconds)))))

  (struct-copy pipeline pipeline-almost [cleaner-thread cleaner-thread]))


;; TODO - orig. pipelines need <() >() redirects, environment modifiers, ...
;; TODO - the structs for pipeline-member-specs should not be exported entirely, only creation functions with #:keyword args, and some inspection functions.
;; TODO - u-pipeline's real implementation should be in a private dir, and the pipeline.rkt should just export some things from it.
