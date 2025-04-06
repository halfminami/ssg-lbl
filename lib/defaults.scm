(define-module lib.defaults
  (export-all))

(select-module lib.defaults)

(define config-in "./content")          ; without /
(define config-out "./_site")
(define config-template #/\+template\.html$/) ; markdown isn't supported!

(define config
  `((in . ,config-in)
    (out . ,config-out)
    (template . ,config-template)))

(define custom-tag
  '((codeblock . ((classname . "hljs")))))

(define default-config
  (hash-table-from-pairs 'eq?
                         `(config . ,config)
                         `(custom-tag . ,custom-tag)))
