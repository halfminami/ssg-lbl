(use gauche.test)
(use file.util)

(use lib.gen)
(use lib.defaults :prefix d:)
(use lib.util :prefix u:)

(define (ls-R path) (directory-fold path cons '()))

(define (test*/diff/directory want got current-dir)
  (let ([want-list     (ls-R want)]
        [got-list      (ls-R got)]
        [remove-prefix (^(pref s) (substring s (string-length pref) -1))])
    (test* "same structure"
           (map (pa$ remove-prefix want) want-list)
           (map (pa$ remove-prefix got) got-list))
    
    (for-each (^(want got)
                (let ([want (string-append "./" (substring want (string-length current-dir) -1))]
                      [got  (string-append "./" (substring got (string-length current-dir) -1))])
                  (test*/diff "same content" `(content-of ,want) `(content-of ,got))))
              want-list
              got-list)))

;; path/content is the inputs
(define (run path)
  (let ([config (hash-table-copy d:default-config)]
        [in   #"./testdata/~|path|/content"]
        [out  #"./testdata/~|path|/got"]
        [want #"./testdata/~|path|/want"])
    (hash-table-update! config
                        'config
                        (^a (u:--> a
                                   (alist-adjoin 'in in)
                                   (alist-adjoin 'out out))))
    (generate in config)
    (test*/diff/directory want out "./testdata/")))

(test-start "generator diff")

(run "template_child_frontmatter")

(test-end :exit-on-failure #t)
