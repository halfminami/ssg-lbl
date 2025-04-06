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
(define (run path :optional [config (hash-table-copy d:default-config)])
  (let ([in   #"./testdata/~|path|/content"]
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

(run "subdir_frontmatter")

(run "path_index")

(run "path_extension")

(run "custom_elements"
     (let1 config (hash-table-copy d:default-config)
       (hash-table-set! config 'custom-tag '((p . ((tagname . "div") (classname . "p")))
                                             (h1 . ((classname . "important dim")))
                                             (blockquote . ((raw . "contenteditable")))
                                             (ul . ((classname . "list")))
                                             (ol . ((classname . "list alphabet")))
                                             (li . ((classname . "item")))
                                             (strong . ((tagname . "b") (classname . "strong")))
                                             (em . ((tagname . "i") (classname . "em")))
                                             (s . ((tagname . "span") (classname . "strike") (raw . "data-rotate=\"130\"")))
                                             (a . ((raw . "target=\"_self\"")))
                                             (code . ((tagname . "samp") (classname . "dark")))
                                             (codeblock . ((classname . "language-plain") (raw . "contenteditable")))
                                             (hr . ((classname . "hr")))))
       config))

(test-end :exit-on-failure #t)
