#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [path-string-symbol? (-> any/c boolean?)]
  [unix-pipeline-member-spec? (-> any/c boolean?)]
  [object-pipeline-member-spec? (-> any/c boolean?)]
  [composite-pipeline-member-spec? (-> any/c boolean?)]

  [rename mk-unix-pipeline-member-spec unix-pipeline-member-spec
          (->* ((listof any/c))
               (#:err (or/c output-port? false/c path-string-symbol?
                            (list/c path-string-symbol?
                                    (or/c 'error 'append 'truncate))
                            pipeline-default-option?)
                #:success (or/c false/c
                                procedure?
                                (listof any/c)
                                pipeline-default-option?)
                )
               unix-pipeline-member-spec?)]
  [object-pipeline-member-spec (-> procedure? object-pipeline-member-spec?)]
  [composite-pipeline-member-spec (-> (listof (or/c unix-pipeline-member-spec?
                                                    object-pipeline-member-spec?
                                                    composite-pipeline-member-spec?))
                                      composite-pipeline-member-spec?)]
  )


 ;; These ones are not really for public consumption.
 pipeline-default-option
 pipeline-default-option?

 )

(module+ internals
  (provide
   (rename-out
    [unix-pipeline-member-spec-argl pipeline-member-spec-argl]
    [unix-pipeline-member-spec-port-err pipeline-member-spec-port-err]
    [unix-pipeline-member-spec-success-pred pipeline-member-spec-success-pred]
    )
   object-pipeline-member-spec-func
   composite-pipeline-member-spec-members
   ))


(struct object-pipeline-member-spec
  (func)
  #:transparent)

(struct composite-pipeline-member-spec
  (members)
  #:transparent)

(struct unix-pipeline-member-spec
  (argl port-err success-pred)
  #:transparent)
(define (mk-unix-pipeline-member-spec argl
                                      #:err [port-err (pipeline-default-option)]
                                      #:success [success-pred (pipeline-default-option)])
  (unix-pipeline-member-spec argl port-err success-pred))

(struct pipeline-default-option ())

(define (path-string-symbol? pss)
  (or (path-string? pss)
      (and (symbol? pss) (path-string? (symbol->string pss)))))
