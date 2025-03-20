;; can I parse markdown line by line, without loading the whole content at once?

(define-module convert.all		; for test
  (export-all))

(define-module convert
  (import convert.all)
  (export markdown->html!))

(select-module convert.all)

;; force output port
(define (display-raw d out) (display d out))

(define (display-raw* . data)
  (define out (last data))
  (for-each (cut display-raw <> out) (drop-right data 1)))

(define (displayln-raw d out)
  (display-raw d out)
  (display-raw #\newline out))

(define (displayln-raw* . data)
  (define out (last data))
  (apply display-raw* data)
  (display-raw #\newline out))

(define (string-prefix? short long)
  (define short-n (string-length short))
  (define long-n (string-length long))
  (and (<= short-n long-n) (string=? short (substring long 0 short-n))))

;; takes care of length when (invalid) input is too short
(define (ignore-first-length ignore-first s)
  (min ignore-first (string-length s)))

(define (ignore-substring ignore-first s)
  (substring s (ignore-first-length ignore-first s) -1))

;; convenience
(define (newline? c) (equal? #\newline c))

;; inside <code> only
(define escape-table
  (hash-table-from-pairs 'eq? '(#\< . "&lt;") '(#\> . "&gt;") #;'(#\& . "&amp;") '(#\" . "&quot;")))
;; should check if it really should be escaped or not
;; for now just avoid &amp; to be able to output &grave;

(define (char->escaped c) (hash-table-get escape-table c c))

;; ---------------------------------------------------------------------------------------------------
;; pre code

;; ``` must exist
(define (next-codeblock line in out env)
  (define ignore-first (hash-table-get env 'ignore-first 0))
  (define this-line (ignore-substring ignore-first line))
  
  (define lang (substring this-line 3 -1)) ; no nesting unlike lists
  (define cls (hash-table-get env 'codeblock-class #f))
  (define clss (append (if cls `(,cls) '())
		       (if (> (string-length lang) 0) `(,#"language-~|lang|") '())))
  (display-raw #"<pre><code class=\"~(string-join clss)\">" out)

  (let1 ignore-first (hash-table-get env 'ignore-first 0)
	(let loop ()
	  (and-let* ([whole-line (read-line in)]
		     [ (not (eof-object? whole-line))]
		     [line (ignore-substring ignore-first whole-line)]
		     [ (not (string=? "```" line))])
	    ($ for-each (cut display-raw <> out) $ map char->escaped $ string->list line)
	    (display-raw #\newline out)
	    (loop))))

  (displayln-raw "</code></pre>" out))

;; ---------------------------------------------------------------------------------------------------
;; ul and ol

;; make procedure that counts first characters whose count? is #t
(define ((count-leading-pred count?) s)
  (define n (string-length s))
  (let loop ([at 0])
    (if (or (<= n at) ($ not $ count? $ string-ref s at))
	at
	(loop (+ 1 at)))))

(define count-leading-spaces
  (count-leading-pred (cut char=? #\space <>)))

(define count-leading-digits
  (count-leading-pred (cut char<=? #\0 <> #\9)))

;; returns next-{ul,ol}!
;; prefix-length returns length of prefix of li
;; if the string is not li then returns #f
(define ((proc-next-list! prefix-length tagname) line in out env)
  (let next-list! ([line line] [in in] [out out] [env env])
    (let* ([ignore-first (hash-table-get env 'ignore-first 0)]
	   [ignored      (ignore-substring ignore-first line)] ; only to check depth
	   [depth        (count-leading-spaces ignored)])
      (display-raw #"<~|tagname|>" out)
      (begin0
       (let loop ([line line] [open #f])
	 (if (not (string? line))
	     (when open (displayln-raw "</li>" out)) ; likely eof == undefined

	     (let* ([ignored    (ignore-substring ignore-first line)]
		    [this-depth (count-leading-spaces ignored)]
		    [this-line  (substring ignored this-depth -1)]
		    [starts     (prefix-length this-line)])
	       (cond [(and (not starts) (= 0 this-depth)) ; out of list block
		      (when open (displayln-raw "</li>" out))
		      line]

		     [(not starts)		  ; not li open
		      (display-raw #\newline out) ; previous content
		      (branch-inline! this-line out env)
		      (loop (read-line in) open)]

		     [(< this-depth depth) ; this depth's end
		      (when open (displayln-raw "</li>" out))
		      line]

		     [(> this-depth depth) ; nest
		      (when open (displayln-raw "</li>" out))
		      (loop (next-list! line in out env) #f)]

		     [else		; new li
		      (when open (displayln-raw "</li>" out))
		      (display-raw "<li>" out)
		      (branch-inline! (substring this-line starts -1) out env)
		      (loop (read-line in) #t)]))))

       (display-raw #"</~|tagname|>" out)))))

(define (ul-prefix-length s)
  (and-let1 _ (or (string-prefix? "+ " s) (string-prefix? "- " s) (string-prefix? "* " s))
	    2))

(define (ol-prefix-length s)
  (and-let* ([n (count-leading-digits s)]
	     [ (> n 0)]
	     [ (string-prefix? ". " (substring s n -1))])
    (+ 2 n)))

;; start of ul. line's prefix is `r/ *[-+] /`
;; returns last line read
(define next-ul! (proc-next-list! ul-prefix-length "ul"))

(define next-ol! (proc-next-list! ol-prefix-length "ol"))

;; ---------------------------------------------------------------------------------------------------

;; blockquote (no space between >s)
(define count-leading->s (count-leading-pred (cut char=? #\> <>)))

(define (q-prefix-length s)
  (and-let* ([n (count-leading->s s)]
	     [ (> n 0)])
    n))

(define |count-leading-#s| (count-leading-pred (cut char=? #\# <>)))

(define (h-prefix-length s)
  (and-let* ([n (|count-leading-#s| s)]
	     [ (> n 0)]
	     [ (string-prefix? " " (substring s n -1))])
    (+ 1 n)))

;; we need to peek instead of read-line
(define (drop-newline! in)
  (while (newline? (peek-char in)) (read-char in)))

(define count-leading--s (count-leading-pred (cut char=? #\- <>)))

;; block level syntax is handled. tags (except <p>) are on stack
(define (markdown->html! in out env)
  (define stack-bottom '())
  
  (define (close-tag data)
    (case (alist-ref data 'tag)
      [(ul ol) (when (alist-ref data 'open) (close-tagname "li"))])
    (close-tagname (alist-ref data 'tagname)))
  
  (define (open-tagname tagname)
    (display-raw #"<~|tagname|>" out))
  (define (close-tagname tagname)
    (displayln-raw #"</~|tagname|>" out))

  (define (pop-stack stack)
    (close-tag (car stack))
    (cdr stack))
  (define (pop-stack-all stack)
    (let loop ([stack stack])
      (if (eq? stack-bottom (car stack))
	  `(,stack-bottom)
	  (begin (close-tag (car stack))
		 (loop (cdr stack))))))

  (define (fresh-env/q q)		; clear inline
    (define ht (hash-table-copy env))
    (hash-table-set! ht 'ignore-first q)
    ht)

  (define (begin-ul! line stack q loop-env
		     spaces this-line-clean
		     ul-sym ul-tagname ol-sym ol-tagname n loop)
    (let* ([top   (car stack)]
	   [tag   (alist-ref top 'tag)]
	   [level (alist-ref top 'level)]
	   [data  `((tag . ,ul-sym) (tagname . ,ul-tagname) (level . ,spaces))])
      (cond
	[(equal? ul-sym tag)		; <ul> is open
	 (cond [(= level spaces)	; new <li>
		(when (alist-ref top 'open) (close-tagname "li"))
		(open-tagname "li")
		(let1 loop-env (fresh-env/q q)
		      (branch-inline! this-line-clean out loop-env)
		      (loop (read-line in) (cons (alist-adjoin top 'open #t)
						 (cdr stack))
			    q loop-env))]
	       [(> level spaces)	; this <ul> end
		(when (alist-ref top 'open) (close-tagname "li"))
		(close-tagname ul-tagname)
		(loop line (cdr stack) q loop-env)]
	       [else			; new <ul>
		(when (alist-ref top 'open) (close-tagname "li"))
		(open-tagname ul-tagname)
		(loop line (cons data (cons (alist-adjoin top 'open #f)
					    (cdr stack)))
		      q (fresh-env/q q))])]
	[(equal? ol-sym tag)		; new <ul> at the same level as this <ol>
	 (when (alist-ref top 'open) (close-tagname "li"))
	 (close-tagname ol-tagname)
	 (open-tagname ul-tagname)
	 (loop line (cons data (cdr stack)) q (fresh-env/q q))]
	[else				; new <ul><li>
	 (let1 next-stack (pop-stack-all stack)
	       (open-tagname ul-tagname) ; new <li> open in next loop
	       (loop line (cons data next-stack)
		     q (fresh-env/q q)))])))

  (let loop ([line (read-line in)] [stack `(,stack-bottom)] [q 0] [loop-env env])
    (if (not (string? line))
        (pop-stack-all stack)
	(let* ([next-q            (or (q-prefix-length line) 0)]
	       [this-line         (substring line next-q -1)]
	       [spaces            (count-leading-spaces this-line)]
	       [this-line-nospace (substring this-line spaces -1)])
	  (cond
	   [(< q next-q)		; open <blockquote>
	    (let1 next-stack (pop-stack-all stack)
		  (open-tagname "blockquote")
		  (let1 next-line (loop line `(,stack-bottom) next-q loop-env)
			(close-tagname "blockquote")
			(loop next-line next-stack q loop-env)))]

	   [(> q next-q)		; close <blockquote>
	    (pop-stack-all stack)
	    line]

	   [(string=? "" line)		; reset, q is zero
	    (let1 next-stack (pop-stack-all stack)
		  (drop-newline! in)
		  (loop (read-line in) next-stack q (fresh-env/q q)))]

	   [(string=? ";;;off" line)	; parser off
	    (until (read-line in)
		   (^s (or (not (string? s)) (string=? ";;;on" s))) => line
		   (displayln-raw line out))
	    (loop (read-line in) stack q loop-env)]

	   [(and (string-prefix? "---" this-line) ; <hr>
		 (= (string-length this-line) (count-leading--s this-line)))
	    (let1 next-stack (pop-stack-all stack)
		  (display-raw "<hr>" out)
		  (loop (read-line in) next-stack q (fresh-env/q q)))]

	   [(string-prefix? "```" this-line) ; <pre><code>
	    (let1 next-stack (pop-stack-all stack)
		  (next-codeblock line in out (fresh-env/q q)) ; blockquote level needed
		  (loop (read-line in) next-stack q (fresh-env/q q)))]

	   [(h-prefix-length this-line) => ; <h1> and friends
	    (^n (let* ([next-stack (pop-stack-all stack)]
		       [tagname #"h~(min 6 (|count-leading-#s| this-line))"])
		  (open-tagname tagname)
		  (branch-inline! (substring this-line n -1) out (fresh-env/q q))
		  (close-tagname tagname)
		  (loop (read-line in) next-stack q (fresh-env/q q))))]

	   [(ul-prefix-length this-line-nospace) =>
	    (^n (begin-ul! line stack q loop-env
			   spaces (substring this-line-nospace n -1)
			   'ul "ul" 'ol "ol" n loop))]
	   [(ol-prefix-length this-line-nospace) =>
	    (^n (begin-ul! line stack q loop-env
			   spaces (substring this-line-nospace n -1)
			   'ol "ol" 'ul "ul" n loop))]
	   
	   [else			; some <p>
	    (let* ([top  (car stack)]
		   [tag  (alist-ref top 'tag)]
		   [data '((tag . p) (tagname . "p"))])
	      (case tag
		[(p)			; <p> already open
		 (display-raw #\newline out)
		 (branch-inline! this-line-nospace out loop-env)
		 (loop (read-line in) stack q loop-env)]
		[(ul ol)		; no p inside li
		 (if (< 0 spaces)	; takes whatever indent as inside li
		     (begin
		       (display-raw #\newline out)
		       (branch-inline! this-line-nospace out loop-env)
		       (loop (read-line in) stack q loop-env))
		     (let1 next-stack (pop-stack-all stack)
			   (open-tagname "p")
			   (branch-inline! this-line-nospace out loop-env)
			   (loop (read-line in) (cons data next-stack) q loop-env)))]
		[else			; new <p>
		 (open-tagname "p")
		 (branch-inline! this-line-nospace out loop-env)
		 (loop (read-line in) (cons data stack) q loop-env)]))])))))

;; TODO: links, tedious to parse when reading line by line
;; TODO: variables
(define (branch-inline! line out env)
  (define n (string-length line))
  (define inline-stack-bottom 'no)
  
  (define (get-inline-stack) (hash-table-get env 'inline-stack `(,inline-stack-bottom)))
  (define (push-inline-stack! sym)
    (hash-table-update! env 'inline-stack (cut cons sym <>) `(,inline-stack-bottom)))
  (define (pop-inline-stack!)
    (hash-table-update! env 'inline-stack cdr))

  (define (make-pos-* ch)
    (^i (if (char=? ch (string-ref line i))
	    (+ 1 i)
	    #f)))
  (define pos-* (make-pos-* #\*))
  (define pos-_ (make-pos-* #\_))

  (define (make-pos-** double)
    (^i (if (and (< (+ 1 i) n) (string=? double (substring line i (+ 2 i))))
	    (+ 2 i)
	    #f)))
  (define pos-** (make-pos-** "**"))
  (define pos-__ (make-pos-** "__"))
  (define pos-~~ (make-pos-** "~~"))

  ;; greedily match
  (define (open-close-something sym tagname acc)
    (^i (let* ([stack (get-inline-stack)]
	       [top   (car stack)])
	  (if (equal? sym top)
	      (begin			; close
		(pop-inline-stack!)
		`(,i ,(cons #"</~|tagname|>" acc)))
	      (begin			; open
		(push-inline-stack! sym)
		`(,i ,(cons #"<~|tagname|>" acc)))))))

  (define (|until-`| i acc should-push)
    (when should-push
      (push-inline-stack! 'code))
    (let loop ([i i] [acc (append (if should-push '("<code>") '()) acc)])
      (if (< i n)
	  (case (string-ref line i)
	    [(#\`)
	     (pop-inline-stack!)
	     `(,(+ 1 i) ,(cons "</code>" acc))]
	    [else => (^c (loop (+ 1 i) (cons (char->escaped c) acc)))]) ; why can't I use cut here
	  `(,i ,acc))))

  (define (|until-}| i acc should-push)
    (if should-push
	(push-inline-stack! 'variable)
	(hash-table-update! env 'variable-name (cut cons #\space <>) '()))
    (let loop ([i i])
      (if (< i n)
	  (case (string-ref line i)
	    [(#\})
	     (pop-inline-stack!)
	     (begin0
	      `(,(+ 1 i) ,(cons (and-let* ([name-rev (hash-table-get env 'variable-name #f)]
					   [name (list->string (reverse name-rev))]
					   [data (alist-ref (hash-table-get env 'user) name)])
				  data)
				acc))
	      (hash-table-set! env 'variable-name '()))]
	    [else => (^c
		      (hash-table-update! env 'variable-name (cut cons c <>) '())
		      (loop (+ 1 i)))])
	  `(,i ,acc))))

  ;; returns next i and acc
  (define (branch! i acc)
    (if (>= i n)
	`(,i ,acc)
	(let ([top (car (get-inline-stack))]
	      [c   (string-ref line i)])
	  (cond [(equal? 'code top)
		 (apply branch! (|until-`| i acc #f))]
		[(equal? 'variable top)
		 (apply branch! (|until-}| i acc #f))]
		[(char=? #\` c)
		 (apply branch! (|until-`| (+ 1 i) acc #t))]
		[(char=? #\{ c)
		 (apply branch! (|until-}| (+ 1 i) acc #t))]
		[(pos-** i) => (open-close-something 'strong-** "strong" acc)]
		[(pos-__ i) => (open-close-something 'strong-__ "strong" acc)]
		[(pos-~~ i) => (open-close-something 'strike-~~ "s"      acc)]
		[(pos-*  i) => (open-close-something 'em-*      "em"     acc)]
		[(pos-_  i) => (open-close-something 'em-_      "em"     acc)]
		[else
		 `(,(+ 1 i) ,(cons (string-ref line i) acc))]))))
  
  (let loop ([i 0] [acc '()])
    (let* ([l   (branch! i acc)]
	   [i   (car l)]
	   [acc (cadr l)])
      (if (< i n)
	  (loop i acc)
	  (for-each (cut display-raw <> out) (reverse acc))))))
