(define-module lib.html-all
  (use lib.util :prefix u:)
  (use lib.markdown.util
       :only (display-raw)
       :prefix util:)
  (export-all))

(define-module lib.html
  (import lib.html-all)
  (export html->html))

(select-module lib.html-all)

;; parses variable only, special treatment for {{content}}
(define (html->html in out env)
  (letrec* ([get-variable
             (^l (let1 name (list->string (reverse l))
                   (if (string=? "content" name)
                     (let1 content! (hash-table-get env 'content!)
                       (content! out env)
                       "")
                     (and-let* ([user (hash-table-get env 'user #f)]
                                [data (alist-ref user name)])
                       data))))]
            [close
             (^(acc)
               (and-let* ([c (read-char in)]
                          [  (char? c)])
                 (cond [(and (char=? #\} c (peek-char in)))
                        (read-char in)
                        (util:display-raw (get-variable acc) out)]
                       [(char=? #\newline c)
                        (close (cons #\space acc))]
                       [else
                        (close (cons c acc))])))]
            [loop
             (^()
               (while (read-char in)
                 char? => c
                 (if (char=? #\{ c (peek-char in))
                   (begin
                     (read-char in)
                     (close '()))
                   (util:display-raw c out))))])
    (loop)))
