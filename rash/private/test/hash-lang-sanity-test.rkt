#lang rash

=quoting-basic-unix-pipe= ls -l =object-pipe= string-upcase =object-pipe= display

(display (rash «=quoting-basic-unix-pipe= ls =object-pipe= string-downcase»))

(rash «(define x 5)
      (define y 6)
      (define z 7)»)

(printf "adding: ~a\n" (+ x y z))

(begin {echo this has a closing delimiter: '"}"})
(begin {echo this has an opening delimiter '"{"})

ls

echo Top-level multiple values:
(values 1 2 3)
