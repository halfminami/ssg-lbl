(use gauche.test)

(use lib.frontmatter)
(import lib.frontmatter.all)

(test-start "lib.frontmatter")

(test-section "eval-string")

(test* "eval" 6 (car (eval-string "(+ 1 2\n3)")))
(test* "bind" '(hello . 42) (car (eval-string "(define (hello) 42)
`(hello . ,(hello))")))

(test-section "read-frontmatter!")

(let* ([in (open-input-string "\
---scm
(define what 'me)
`((make . hash-table) (from . ,what))
---
# yo")]
       [res (read-frontmatter! in)]
       [data (car res)]
       [in (cadr res)])
  (test* "eval" '((make . hash-table) (from . me)) (car data))
  (test* "eval stops" "# yo" (read-line in))
  (close-input-port in))

(let* ([in (open-input-string "There's no frontmatter!")]
       [res (read-frontmatter! in)]
       [data (car res)]
       [in (cadr res)])
  (test* "no frontmatter" #f data)
  (test* "don't read" "There's no frontmatter!" (read-line in))
  (close-input-port in))

(test-end :exit-on-failure #t)
