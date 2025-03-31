#!/usr/bin/env gosh

(define config
  '((in . "./content")
    (out . "./_site")
    (template . "+template.html")))

(define custom-tag '())

(define config-ht
  (hash-table-from-pairs 'eq?
                         `(config . ,config)
                         `(custom-tag . ,custom-tag)))

(define (main args) (print args "?"))
