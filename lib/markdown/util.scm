(define-module lib.markdown.util
  (use lib.util :prefix u:)

  (export ignore-substring
	  char->escaped
	  drop-newline!

	  count-leading-spaces
	  count-leading-digits
	  count-leading->s
	  |count-leading-#s|
	  count-leading--s

	  ul-prefix-length
	  ol-prefix-length
	  q-prefix-length
	  h-prefix-length

	  rev-list->string

	  display-raw display-raw*
	  displayln-raw displayln-raw*

	  get-custom-tag
	  format-close-tag format-open-tag
	  addto-raw addto-classname

	  header->id))

(select-module lib.markdown.util)

;; ---------------------------------------------------------------------------------------------------
;; string convenience

;; takes care of length when (invalid) input is too short
(define (ignore-first-length ignore-first s)
  (min ignore-first (string-length s)))

(define (ignore-substring ignore-first s)
  (substring s (ignore-first-length ignore-first s) -1))

;; inside <code> only
(define escape-table
  (hash-table-from-pairs 'eq? '(#\< . "&lt;") '(#\> . "&gt;") #;'(#\& . "&amp;") '(#\" . "&quot;")))
;; should check if it really should be escaped or not
;; for now just avoid &amp; to be able to output &grave;

;; we need to peek instead of read-line
(define (drop-newline! in) (while (u:newline? (peek-char in)) (read-char in)))

(define (char->escaped c) (hash-table-get escape-table c c))

(define count-leading-spaces
  (u:count-leading-pred (pa$ char=? #\space)))

(define count-leading-digits
  (u:count-leading-pred (cut char<=? #\0 <> #\9)))

;; blockquote (no space between >s)
(define count-leading->s (u:count-leading-pred (pa$ char=? #\>)))

(define |count-leading-#s| (u:count-leading-pred (pa$ char=? #\#)))

(define count-leading--s (u:count-leading-pred (pa$ char=? #\-)))

(define (xs->string xs)
  ($ string-append $* map x->string xs))

(define (rev-list->string l)
  ($ xs->string $ reverse l))

;; ---------------------------------------------------------------------------------------------------
;; deciding which markdown element we are in

(define (ul-prefix-length s)
  (and (or (u:string-prefix? "+ " s) (u:string-prefix? "- " s) (u:string-prefix? "* " s))
       2))

(define (ol-prefix-length s)
  (and-let* ([n (count-leading-digits s)]
	     [  (> n 0)]
	     [  (u:string-prefix? ". " (substring s n -1))])
    (+ 2 n)))

(define (q-prefix-length s)
  (and-let* ([n (count-leading->s s)]
	     [  (> n 0)])
    n))

(define (h-prefix-length s)
  (and-let* ([n (|count-leading-#s| s)]
	     [  (> n 0)]
	     [  (u:string-prefix? " " (substring s n -1))])
    (+ 1 n)))

;; ---------------------------------------------------------------------------------------------------
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

;; ---------------------------------------------------------------------------------------------------
;; HTML output format

(define (get-custom-tag env sym)
  (if-let1 data (alist-ref (hash-table-get env 'custom-tag '()) sym)
    (let* ([data (alist-adjoin data 'tag sym)] ; if 'tag is changed, it will break
	   [data (alist-update-in data '(tagname) identity equal? (symbol->string sym))])
      data)
    `((tagname . ,(symbol->string sym)) (tag . ,sym))))

(define (format-close-tag data) #"</~(alist-ref data 'tagname)>")

(define (format-open-tag data)
  (let* ([tagname   (alist-ref data 'tagname)]
	 [classname (alist-ref data 'classname)]
	 [raw       (alist-ref data 'raw)]
	 [names     (append `(,tagname)
			    (if classname `(,#"class=\"~|classname|\"") '())
			    (if raw `(,raw) '()))]
	 [str       (string-join names)])
    #"<~|str|>"))

;; append string. avoid extra space
(define (make-addto syms)
  (lambda (data str)
    (alist-update-in data
		     syms
		     (^l (if l (string-append l " " str) str))
		     equal?
		     #f)))

(define addto-raw (make-addto '(raw)))
(define addto-classname (make-addto '(classname)))

(define id-replace (hash-table-from-pairs 'eq? '(#\space . #\-)))

;; spaces are replaced by -
(define (header->id str)
  ($ list->string
     $ map char-downcase
     $ map (^c (hash-table-get id-replace c c))
     $ string->list str))
