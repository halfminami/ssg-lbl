(define-module lib.frontmatter.all
  (use lib.util :prefix u:)
  (export-all))

(define-module lib.frontmatter
  (import lib.frontmatter.all)
  (export read-frontmatter!))

(select-module lib.frontmatter.all)

(define (eval-string str m)
  `(,(call-with-input-string str
       (^i (let loop ([prev #f])
             (if (u:eof-reached? i)
               prev
               (loop (or (eval (read i) m) prev))))))
    ,m))

;; returns list (data in)
;; if there's frontmatter, consumes it and data is (value module)
;; else, data is #f
(define (read-frontmatter! in m)
  (let1 line (read-line in)
    (if (string=? "---scm" line)
      (let1 acc '()
        (while (read-line in)
          (^s (and (string? s) (not (string=? "---" s)))) => line
          (push! acc line))
        `(,(eval-string (string-join (reverse acc) "\n") m)
          ,in))
      `(#f ,(u:unget-line line in)))))
