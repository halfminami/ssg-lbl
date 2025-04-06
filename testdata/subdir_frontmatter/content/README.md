---scm
(define children (hash-table-get env 'children '()))
(define posts (alist-ref children "./testdata/subdir_frontmatter/content/posts"))

(define output-paths ($ string-join
                      $ map (cut alist-ref <> 'output-path)
                      $ map (cut hash-table-get <> 'special '()) posts))
`((output-paths . ,output-paths))
---
Parsed files should be able to access frontmatter of files in its subdirectory.

Here is `output-paths`: {{output-paths}}
