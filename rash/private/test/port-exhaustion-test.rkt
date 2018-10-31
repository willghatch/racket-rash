#lang rash

;; I had forgotten to close some output ports in places, which caused port exhaustion, which caused new subprocess creation to fail.  This is a little sanity check to be sure that's not happening.

(module+ non-sandboxed-test
  {
   (require rackunit basedir)
   ;; I should find out what the real limit is,
   ;; but this is enough to trip the bad behavior but not take forever.
   (define exhaustion-number 2000)
   (define test-dir (writable-runtime-dir
                     #:program "rash-package-test/port-exhaustion"))

   (check-not-exn
    (λ ()
      (for ([i (in-range exhaustion-number)])
        #{dirname foo/bar |>> (λ(x)x)})))
   (check-not-exn
    (λ ()
      (for ([i (in-range exhaustion-number)])
        #{dirname foo/bar})))


   mkdir -p $test-dir
   echo test file contents &>! $test-dir/file1

   (check-not-exn
    (λ ()
      (for ([i (in-range exhaustion-number)])
        {cat - &< $test-dir/file1 &>! $test-dir/file2})))
   (check-equal?
    #{cat $test-dir/file1}
    #{cat $test-dir/file2})

   rm -rf $test-dir
   }
  )
