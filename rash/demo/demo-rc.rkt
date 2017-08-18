#lang racket/base

#|
This is a demo file that could be required by `rashrc` or `rashrc.rkt`.

To use it, put the following in `~/.config/rash/rashrc`:

(require rash/demo/demo-rc.rkt)
(set-default-pipeline-starter! \|)

|#

(require rash/demo/setup)
(provide (all-from-out rash/demo/setup))

(define real-stderr (current-error-port))
(current-error-port (highlighting-output-port (current-output-port)))

