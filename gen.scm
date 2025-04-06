#!/bin/sh
#|
exec gosh -I. -- $0 "$@"
|#
;; chmod a+x gen.scm

(use lib.gen)
(use lib.defaults :prefix d:)
(use lib.commandopt)

(use gauche.parseopt)
(use srfi.152)

(load "./config.scm")

(define (usage name)
  (display #"\
usage: ~|name| [OPTION] ...
  DIY static site generator

[OPTION] is one of:
  -i, --in=<path>
    input files path (default ~|d:config-in|)
  -o, --out=<path>
    output files path (default ~|d:config-out|)
  -v, --verbose
    more log messages
  -h, --help
    print this help message and exit
"
           (current-output-port))
  (exit #f))

;; TODO: better error handling
(define (main args)
  (let-args (cdr args)
      ([in  "i|in=s"    d:config-in => (cut string-trim-right <> #\/)]
       [out "o|out=s"   d:config-out => (cut string-trim-right <> #\/)]
       [#f  "v|verbose" #f => (cut verbose #t)]
       [#f  "h|help"    => (cut usage (car args))]
       [else (opt . _) (print "Unknown option : " opt) (usage (car args))])
    (define ht (hash-table-copy d:default-config))
    (hash-table-update! ht 'config (cut alist-adjoin <> 'in in))
    (hash-table-update! ht 'config (cut alist-adjoin <> 'out out))
    (verbose-print #"using 'config ~(hash-table-get ht 'config)")
    (generate in ht)))
