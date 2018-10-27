#lang rash

;; I had forgotten to close some output ports in places, which caused port exhaustion, which caused new subprocess creation to fail.  This is a little sanity check to be sure that's not happening.

(module+ non-sandboxed-test
  (require rackunit)
  ;; I should find out what the real limit is,
  ;; but this is enough to trip the bad behavior but not take forever.
  (define exhaustion-number 2000)
  (check-not-exn
   (λ ()
     (for ([i (in-range exhaustion-number)])
       #{dirname foo/bar |>> (λ(x)x)})))
  (check-not-exn
   (λ ()
     (for ([i (in-range exhaustion-number)])
       #{dirname foo/bar})))
  )
