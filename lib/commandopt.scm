;; verbose macro

(define-module lib.commandopt
  (export-all))

(select-module lib.commandopt)

(define verbose (make-parameter #f))

(define (verbose-print mes)
  (when (verbose)
    (display (string-append mes "\n") (current-error-port))))
