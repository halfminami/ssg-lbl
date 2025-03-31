(use gauche.test)

(use lib.frontmatter)
(import lib.frontmatter.all)

(test-start "lib.frontmatter")

(test-section "eval-string")

(test* "eval" 6 (car (eval-string "(+ 1 2\n3)" (make-module #f))))
(test* "bind" '(hello . 42) (car (eval-string "(define (hello) 42)
`(hello . ,(hello))"
                                              (make-module #f))))

(let* ([env1 (hash-table-from-pairs 'eq? '(special . ((path . "./content/stuff/child1.md"))))]
       [env2 (hash-table-from-pairs 'eq? '(special . ((path . "./content/stuff/child2.md"))))]
       [env3 (hash-table-from-pairs 'eq? '(special . ((path . "./content/stuff/child3.md"))))]

       [ch `(,env1 ,env2 ,env3)]
       [m  (make-module #f)]

       [fm "\
(define paths ($ map (cut alist-ref <> 'path)
               $ map (cut hash-table-ref <> 'special) children))
(define lis ($ apply string-append $ map (^s #\"<li>~|s|</li>\") paths))
(define html #\"<ul>~|lis|</ul>\")
`((raw-paths . ,html))"])
  (eval `(define children ',ch) m)
  (test* "children env"
         '((raw-paths . "<ul><li>./content/stuff/child1.md</li><li>./content/stuff/child2.md</li><li>./content/stuff/child3.md</li></ul>"))
         (car (eval-string fm m))))

(test-section "read-frontmatter!")

(let* ([in (open-input-string "\
---scm
(define what 'me)
`((make . hash-table) (from . ,what))
---
# yo")]
       [res  (read-frontmatter! in (make-module #f))]
       [data (car res)]
       [in   (cadr res)])
  (test* "eval" '((make . hash-table) (from . me)) (car data))
  (test* "eval stops" "# yo" (read-line in))
  (close-input-port in))

(let* ([in   (open-input-string "There's no frontmatter!")]
       [res  (read-frontmatter! in (make-module #f))]
       [data (car res)]
       [in   (cadr res)])
  (test* "no frontmatter" #f data)
  (test* "don't read" "There's no frontmatter!" (read-line in))
  (close-input-port in))

(test-end :exit-on-failure #t)
