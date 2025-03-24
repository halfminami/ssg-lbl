;; can I parse markdown line by line, without loading the whole content at once?

(define-module lib.markdown-all		; for test
  (use lib.util :prefix u:)
  (use lib.markdown.util :prefix util:)

  (use lib.markdown.inline :only (branch-inline!))

  (export-all))

(define-module lib.markdown
  (import lib.markdown-all)
  (export markdown->html!))

(select-module lib.markdown-all)

;; ---------------------------------------------------------------------------------------------------
;; handle block level elements: ul, li, p, ...

;; ``` must exist
;; codeblock's tagname is ignored!
(define (next-codeblock line in out env)
  (define ignore-first (hash-table-get env 'q 0))
  (define this-line (util:ignore-substring ignore-first line))
  (define lang (substring this-line 3 -1)) ; no nesting unlike lists

  (define data-code (u:as-> (util:get-custom-tag env 'codeblock) d
			    (alist-adjoin d 'tagname "code") ; configures code
			    (if (u:string-empty? lang)
			      d
			      (util:addto-classname d #"language-~|lang|"))))

  (util:display-raw #"<pre>~(util:format-open-tag data-code)" out)

  (let loop ()
    (and-let* ([whole-line (read-line in)]
	       [           (not (eof-object? whole-line))]
	       [line       (util:ignore-substring ignore-first whole-line)]
	       [           (not (string=? "```" line))])
      ($ for-each (cut util:display-raw <> out)
	 $ map util:char->escaped
	 $ string->list line)
      (util:display-raw #\newline out)
      (loop)))

  (util:displayln-raw "</code></pre>" out))

;; env is a hash-table with keys:
;; - 'user: alist, contains variables (coming from frontmatter)
;; - 'custom-tag: alist of default element names,
;;   contains alist holding 'tagname, 'classname (string), and 'raw (arbitrary string inside open tag)
;; block level syntax is handled here. tags (except <p>) are on stack
(define (markdown->html! in out env)
  (define headings '())
  
  (define stack-bottom '())
  (define empty-stack `(,stack-bottom))

  (define (tag-data sym) (util:get-custom-tag env sym))

  ;; takes care of lists <li>
  ;; after this called stack may be popped
  (define (close-tag data)
    (case (alist-ref data 'tag)
      [(ul ol) (when (alist-ref data 'open)
		 (close-tag (tag-data 'li)))])
    (util:displayln-raw (util:format-close-tag data) out))

  (define (open-tag data) (util:display-raw (util:format-open-tag data) out))

  (define (pop-stack stack)
    (close-tag (car stack))
    (cdr stack))
  (define (pop-stack-all stack)
    (let loop ([stack stack])
      (if (eq? stack-bottom (car stack))
	empty-stack
	(begin (close-tag (car stack))
	       (loop (cdr stack))))))

  (define (fresh-env/q q)		; clear inline
    (define ht (hash-table-copy env))
    (hash-table-set! ht 'q q)
    ht)

  ;; id doesn't get evaluated, don't use variables inside headings!
  (define (handle-heading! stack line-noq q loop)
    (^n (let* ([stack           (pop-stack-all stack)]
	       [tag             (string->symbol #"h~(min 6 (|util:count-leading-#s| line-noq))")]
	       [line-noq-nopref (substring line-noq n -1)]
               [id              (util:header->id line-noq-nopref)]
	       [h-data          (u:--> (tag-data tag)
	                               (util:addto-raw #"id=\"~|id|\""))])
          (push! headings `(,tag . ,id))
	  (open-tag h-data)
	  (branch-inline! line-noq-nopref out (fresh-env/q q))
	  (close-tag h-data)
	  (loop (read-line in) stack (fresh-env/q q)))))

  ;; the line's prefix is ul
  (define (begin-ul! line stack loop-env fresh-env loop ; loop environment
		     spaces		; for list level
		     line-clean		; no space, maybe printed out
		     ul-sym ol-sym)
    (let* ([top           (car stack)]
	   [tag           (alist-ref top 'tag)]
	   [level         (alist-ref top 'level)]
	   [data-this-ul  (alist-adjoin (tag-data ul-sym) 'level spaces)]

	   [close-ul (cut close-tag (tag-data ul-sym))]
	   [close-ol (cut close-tag (tag-data ol-sym))]
	   [close-li (cut close-tag (tag-data 'li))]
	   [open-ul  (cut open-tag  (tag-data ul-sym))]
	   [open-ol  (cut open-tag  (tag-data ol-sym))]
	   [open-li  (cut open-tag  (tag-data 'li))]

	   [new-ul-loop (u:thunk
			 (open-ul)
			 (let1 stack (u:->> (cdr stack)
					    (cons (alist-adjoin top 'open #f)) ; li closed
					    (cons data-this-ul))
			   (loop line stack (fresh-env))))])

      (when (alist-ref top 'open) (close-li))
      (cond
       [(equal? ul-sym tag)		; <ul> is open for this level
	(cond [(= level spaces)         ; new <li>
	       (open-li)
	       (let ([loop-env (fresh-env)] ; preserve inline format for newlines
		     [stack    (u:->> (cdr stack)
				      (cons (alist-adjoin top 'open #t)))])
		 (branch-inline! line-clean out loop-env)
		 (loop (read-line in) stack loop-env))]
	      [(> level spaces)         ; this <ul> end
	       (close-ul)
	       (loop line (cdr stack) loop-env)]
	      [else			; new <ul>
	       (new-ul-loop)])]
       [(equal? ol-sym tag)
	(cond [(= level spaces) ; new <ul> at the same level as this <ol>
	       (close-ol)
	       (open-ul)
	       (loop line (cons data-this-ul (cdr stack)) (fresh-env))]
	      [(> level spaces)         ; this <ol> end
	       (close-ol)
	       (loop line (cdr stack) loop-env)]
	      [else
	       (new-ul-loop)])]
       [else				; new <ul><li>
	(let1 stack (pop-stack-all stack)
	  (open-ul)               ; new <li> will be open in next loop
	  (loop line (cons data-this-ul stack) (fresh-env)))])))

  (let loop ([line (read-line in)] [stack empty-stack] [loop-env env])
    (if (not (string? line))
      (pop-stack-all stack)
      (let* ([q             (hash-table-get loop-env 'q 0)]
	     [next-q        (or (util:q-prefix-length line) 0)]
	     [line-noq      (substring line next-q -1)] ; without blockquote >s
	     [spaces        (util:count-leading-spaces line-noq)]
	     [line-noq-nosp (substring line-noq spaces -1)]) ; without leading space
	(cond
	 [(< q next-q)                  ; open <blockquote>
	  (let1 stack (pop-stack-all stack)
	    (open-tag (tag-data 'blockquote))
	    (let1 line (loop line empty-stack (fresh-env/q next-q))
	      (close-tag (tag-data 'blockquote))
	      (loop line stack (fresh-env/q q))))]

	 [(> q next-q)                  ; close <blockquote>
	  (pop-stack-all stack)
	  line]

	 ;; blockquote level ok
	 [(string=? "" line)		; reset, q is zero
	  (let1 stack (pop-stack-all stack)
	    (util:drop-newline! in)
	    (loop (read-line in) stack (fresh-env/q q)))]

	 [(string=? ";;;off" line)	; parser off
	  (until (read-line in)
	    (^s (or (not (string? s)) (string=? ";;;on" s))) => line
	    (util:displayln-raw line out))
	  (loop (read-line in) stack loop-env)]

	 [(and (u:string-prefix? "---" line-noq) ; <hr>
	       (= (string-length line-noq) (util:count-leading--s line-noq)))
	  (let1 stack (pop-stack-all stack)
	    (open-tag (tag-data 'hr))
	    (loop (read-line in) stack (fresh-env/q q)))]

	 [(u:string-prefix? "```" line-noq) ; <pre><code>
	  (let1 stack (pop-stack-all stack)
	    (next-codeblock line in out (fresh-env/q q)) ; blockquote level needed
	    (loop (read-line in) stack (fresh-env/q q)))]

	 [(util:h-prefix-length line-noq) => ; <h1> and friends
	  (handle-heading! stack line-noq q loop)]

	 [(util:ul-prefix-length line-noq-nosp) =>
	  (^n (begin-ul! line stack loop-env (cut fresh-env/q q) loop
			 spaces (substring line-noq-nosp n -1)
			 'ul 'ol))]
	 [(util:ol-prefix-length line-noq-nosp) =>
	  (^n (begin-ul! line stack loop-env (cut fresh-env/q q) loop
			 spaces (substring line-noq-nosp n -1)
			 'ol 'ul))]

	 [else                          ; some <p>
	  (let* ([top  (car stack)]
		 [tag  (alist-ref top 'tag)]
		 [data (tag-data 'p)])
	    (case tag
	      [(p)			; <p> already open
	       (util:display-raw #\newline out)
	       (branch-inline! line-noq out loop-env)
	       (loop (read-line in) stack loop-env)]
	      [(ul ol)                  ; no p inside li
	       (if (< 0 spaces)
		 (begin           ; takes whatever indent as inside li
		   (util:display-raw #\newline out)
		   (branch-inline! line-noq-nosp out loop-env)
		   (loop (read-line in) stack loop-env))
		 (let1 stack (pop-stack-all stack)
		   (open-tag (tag-data 'p))
		   (branch-inline! line-noq-nosp out loop-env)
		   (loop (read-line in) (cons data stack) loop-env)))]
	      [else			; new <p>
	       (open-tag (tag-data 'p))
	       (branch-inline! line-noq out loop-env)
	       (loop (read-line in) (cons data stack) loop-env)]))]))))

  (reverse headings))
