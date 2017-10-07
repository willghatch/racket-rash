#lang racket/base

#|
This is a demo file that could be required by `rashrc` or `rashrc.rkt`.

To use it, put the following in `~/.config/rash/rashrc`:

(require rash/demo/demo-rc.rkt)
set-default-pipeline-starter! |

|#

(provide
 (all-from-out rash/demo/setup)
 (all-from-out rash/private/rashrc-lib)
 (all-from-out rash/demo/ps-wrapper)
 )

(require
 rash/demo/setup
 rash/private/rashrc-lib
 rash/demo/ps-wrapper
 )

(define real-stderr (current-error-port))
(current-error-port (highlighting-output-port (current-output-port)))
(current-result-print-default-function
 (print-table-list-specially
  (current-result-print-default-function)))

