#!/usr/bin/env gosh

(define config
  (hash-table-from-pairs 'equal? '(dir "content")))

(define (main args) (print args "?"))
