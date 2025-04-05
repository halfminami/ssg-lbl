#!/usr/bin/env gosh

(use lib.gen)
(use lib.defaults :prefix d:)

(load "./config.scm")

(define (main args)
  (let* ([conf (hash-table-get config 'config '())]
         [in   (alist-ref conf 'in eq? d:config-in)])
    (generate in config)))
