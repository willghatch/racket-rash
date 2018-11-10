#lang rash

(module+ non-sandboxed-test
  {
   (require rackunit basedir)
   ;; I should find out what the real limit is,
   ;; but this is enough to trip the bad behavior but not take forever.
   (define test-dir (writable-runtime-dir
                     #:program "rash-package-test/unix-pipe-misc"))


   (define p+ep-out-str  "p+ep stdout")
   (define p+ep-err-str  "p+ep stderr")
   (define (p+ep)
     (printf p+ep-out-str)
     (eprintf p+ep-err-str)
     )

   mkdir -p $test-dir

   $p+ep #:e>! $test-dir/p+ep-err &>! $test-dir/p+ep-out

   (check-equal?
    #{cat $test-dir/p+ep-err}
    p+ep-err-str)
   (check-equal?
    #{cat $test-dir/p+ep-out}
    p+ep-out-str)

   (check-not-exn (λ () {echo hi &> $test-dir/out1}))
   (check-exn exn? (λ () {echo hi &> $test-dir/out1}))
   (check-not-exn (λ () {echo hi &>! $test-dir/out1}))
   (check-equal? #{cat $test-dir/out1} "hi")
   (check-not-exn (λ () {echo bye &>> $test-dir/out1}))
   (check-equal? #{cat $test-dir/out1} "hi\nbye")

   (check-not-exn (λ () {$p+ep #:e> $test-dir/err1 &>! $test-dir/out1}))
   (check-equal? #{cat $test-dir/err1} p+ep-err-str)
   (check-exn exn? (λ () {$p+ep #:e> $test-dir/err1 &>! $test-dir/out1}))
   (check-equal? #{cat $test-dir/err1} p+ep-err-str)
   (check-not-exn (λ () {$p+ep #:e>! $test-dir/err1 &>! $test-dir/out1}))
   (check-equal? #{cat $test-dir/err1} p+ep-err-str)
   (check-not-exn (λ () {$p+ep #:e>> $test-dir/err1 &>! $test-dir/out1}))
   (check-equal? #{cat $test-dir/err1} (string-append p+ep-err-str p+ep-err-str))

   rm -rf $test-dir
   }
  )
