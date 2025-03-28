(define-module lib.markdown.inline-all
  (use lib.util :prefix u:)
  (use lib.markdown.util :prefix util:)
  (export-all))

(define-module lib.markdown.inline
  (import lib.markdown.inline-all)
  (export branch-inline!))

(select-module lib.markdown.inline-all)

;; links are tedious to parse
;; I couldn't parse ![...](...) without looking ahead, ! is so common
;; there is untested remnants, it's harmless and I may be able to choose different syntax

(define (branch-inline! line out env)
  (define n (string-length line))
  (define inline-stack-bottom 'no)

  ;; stack contains only symbols
  (define (get-inline-stack) (hash-table-get env 'inline-stack `(,inline-stack-bottom)))
  (define (push-inline-stack! sym)	; always car'ed
    (hash-table-update! env 'inline-stack (pa$ cons sym) `(,inline-stack-bottom)))
  (define (pop-inline-stack!) (hash-table-pop! env 'inline-stack))

  ;; after pop
  ;; when [here](), text should put inside 'a-text or 'img-text, accordingly
  (define (push-or-acc! c acc)
    (if-let1 sym (find (cut u:equal-one-of? <> 'a-text 'img-text)
		       (get-inline-stack))
      (begin (hash-table-push! env sym c)
	     acc)
      (cons c acc)))

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

  (define |pos-{{| (make-pos-** "{{"))
  (define |pos-}}| (make-pos-** "}}"))

  ;; greedily match
  (define (open-close-something sym tag acc)
    (^i (let* ([stack (get-inline-stack)]
	       [top   (car stack)])
	  (if (equal? sym top)
	    (begin			; close
	      (pop-inline-stack!)
	      `(,i ,(push-or-acc! (util:format-close-tag (util:get-custom-tag env tag)) acc)))
	    (begin			; open
	      (push-inline-stack! sym)
	      `(,i ,(push-or-acc! (util:format-open-tag (util:get-custom-tag env tag)) acc)))))))

  (define (|until-`| i acc should-push)
    (when should-push
      (push-inline-stack! 'code))
    (let1 acc (push-or-acc! (if should-push
			      (util:format-open-tag (util:get-custom-tag env 'code))
			      "")
			    acc)
      (let loop ([i i] [acc acc])
	(if (< i n)
	  (case (string-ref line i)
	    [(#\`)
	     (pop-inline-stack!)
	     `(,(+ 1 i) ,(push-or-acc! (util:format-close-tag (util:get-custom-tag env 'code)) acc))]
	    [else			; why can't I use cut here
	     => (^c
		 (loop (+ 1 i)
		       (push-or-acc! (util:char->escaped c) acc)))])
	  `(,i ,acc)))))

  (define (|something-until-)|
	   a-link-sym			; on stack
	   a-text-sym a-url-sym		; env
	   format)			; takes text and url
    (lambda (i acc should-push)
      (when should-push
	(push-inline-stack! a-link-sym))
      (let loop ([i i])
	(if (< i n)
	  (case (string-ref line i)
	    [(#\))
	     (pop-inline-stack!)
	     (begin0
		 `(,(+ 1 i)
		   ,(push-or-acc! (let ([text (util:rev-list->string (hash-table-get env a-text-sym))]
				        [url  (util:rev-list->string (hash-table-get env a-url-sym))])
				    (format text url))
				  acc))
	       (hash-table-delete! env a-text-sym)
	       (hash-table-delete! env a-url-sym))]
	    [else => (^c (hash-table-push! env a-url-sym c)
			 (loop (+ 1 i)))])
	  `(,i ,acc)))))

  ;; I probably should create symbol from string..
  (define |a-until-)|
    (|something-until-)|
     'a-link 'a-text 'a-url
     (lambda (text url)
       (let* ([url-format #"href=\"~|url|\""]
	      ;; add to 'raw
	      [data (u:--> (util:get-custom-tag env 'a)
			   (util:addto-raw url-format))]

	      [open  (util:format-open-tag data)]
	      [close (util:format-close-tag data)])
	 (string-append open text close)))))

  ;; self-closing, classname and raw only
  (define |img-until-)| (|something-until-)|
			 'img-link 'img-text 'img-url
			 (lambda (text url) #"<img src=\"~|url|\" alt=\"~|text|\">")))

  ;; []here(), link parse fail
  (define (a-img-rewind! c acc sym)
    (ecase sym
      [(a-text-closed)
       (let* ([text      (util:rev-list->string (hash-table-get env 'a-text))]
	      [formatted #"[~|text|]~|c|"])
	 (pop-inline-stack!)
	 (hash-table-delete! env 'a-text)
	 (cons formatted acc))]
      [(img-text-closed)
       (let* ([text      (util:rev-list->string (hash-table-get env 'img-text))]
	      [formatted #"![~|text|]~|c|"])
	 (pop-inline-stack!)
	 (hash-table-delete! env 'img-text)
	 (cons formatted acc))]))

  (define (|a-img-(-should-follow?|)
    (u:equal-one-of? (car (get-inline-stack)) 'a-text-closed 'img-text-closed))
  
  (define (|until-}}| i acc should-push)
    (when should-push
      (push-inline-stack! 'variable))
    (let loop ([i i])
      (cond [(>= i n) `(,i ,acc)]
            [(|pos-}}| i) =>
             (^i
	      (pop-inline-stack!)
              (begin0
                  `(,i
                    ,(push-or-acc! (and-let* ([name-rev (hash-table-get env 'variable-name #f)]
					      [name     (list->string (reverse name-rev))]
					      [user     (hash-table-get env 'user #f)]
					      [data     (alist-ref user name)])
				     data)
			           acc))
                (hash-table-delete! env 'variable-name)))]
            [(string-ref line i) =>
             (^c (hash-table-push! env 'variable-name c)
		 (loop (+ 1 i)))])))

  ;; returns next i and acc
  (define (branch! i acc)
    (if (>= i n)
      `(,i ,acc)
      (let ([top         (car (get-inline-stack))]
	    [c           (string-ref line i)]
	    [may-rewind! (^ (c a-until sym)
			   (if (char=? #\( c)
			     (begin
			       (pop-inline-stack!)
			       (apply branch! (a-until (+ 1 i) acc #t)))
			     `(,(+ 1 i) ,(a-img-rewind! c acc sym))))])
	(case top
	  ;; characters that have to immediately follow
	  [(a-text-closed)
	   (may-rewind! c |a-until-)| 'a-text-closed)]
	  [(img-text-closed)
	   (may-rewind! c |img-until-)| 'img-text-closed)]
	  [(!)
	   (if (char=? #\[ c)
	     (begin
	       (pop-inline-stack!)
	       (push-inline-stack! 'img-text)
	       `(,(+ 1 i) ,acc))
	     (begin
	       (pop-inline-stack!)
	       `(,(+ 1 i) ,(push-or-acc! "![" acc))))]

	  ;; verbatim
	  [(code)     (apply branch! (|until-`| i acc #f))]
	  [(a-link)   (apply branch! (|a-until-)| i acc #f))]
	  [(img-link) (apply branch! (|img-until-)| i acc #f))]
          [(variable)
           (hash-table-push! env 'variable-name #\space) ; treat newline as space
           (apply branch! (|until-}}| i acc #f))]
	  [else
	   (cond [(char=? #\` c)
		  (apply branch! (|until-`| (+ 1 i) acc #t))]
                 [(|pos-{{| i) => (^i (apply branch! (|until-}}| i acc #t)))]
		 ;; url text
		 [(and (char=? #\[ c)	; don't nest
		       (not (u:equal-one-of? top 'a-text 'img-text)))
		  (push-inline-stack! 'a-text)
		  `(,(+ 1 i) ,acc)]
		 ;; img
		 #;
		 [(and (char=? #\! c)
		       (not (u:equal-one-of? top 'a-text 'img-text)))
		  (push-inline-stack! '!)
		  `(,(+ 1 i) ,acc)]
		 ;; close url text
		 [(and (char=? #\] c) (equal? 'a-text top))
		  (pop-inline-stack!)
		  (push-inline-stack! 'a-text-closed)
		  `(,(+ 1 i) ,acc)]
		 [(and (char=? #\] c) (equal? 'img-text top))
		  (pop-inline-stack!)
		  (push-inline-stack! 'img-text-closed)
		  `(,(+ 1 i) ,acc)]

		 ;; decoration
		 [(pos-** i) => (open-close-something 'strong-** 'strong acc)]
		 [(pos-__ i) => (open-close-something 'strong-__ 'strong acc)]
		 [(pos-~~ i) => (open-close-something 'strike-~~ 's      acc)]
		 [(pos-*  i) => (open-close-something 'em-*      'em     acc)]
		 [(pos-_  i) => (open-close-something 'em-_      'em     acc)]

		 [else
		  `(,(+ 1 i) ,(push-or-acc! c acc))])]))))

  (let loop ([i 0] [acc '()])
    (let* ([l   (branch! i acc)]
	   [i   (car l)]
	   [acc (cadr l)])
      (if (< i n)
	(loop i acc)
	(begin
	  ;; newline inside link text
	  (if-let1 sym (find (cut u:equal-one-of? <> 'a-text 'img-text)
			     (get-inline-stack))
	    (hash-table-push! env sym #\newline))
	  ($ for-each
	     (cut util:display-raw <> out)
	     $ reverse
	     (if-let1 sym (|a-img-(-should-follow?|)
	       (a-img-rewind! #\newline acc sym)
	       acc)))))))
