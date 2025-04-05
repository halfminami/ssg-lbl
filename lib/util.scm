(define-module lib.util
  (use gauche.vport)
  
  (export thunk
	  
	  -->
	  ->>
	  as->

	  equal-one-of?

	  string-prefix?
	  count-leading-pred
	  newline?
	  string-empty?

          eof-reached?
          input-port-append
          unget-line

          hash-table/alist-union!))

(select-module lib.util)

;; ---------------------------------------------------------------------------------------------------
;; convenience macros

(define-syntax thunk
  (syntax-rules () [(_ expr ...) (cut begin expr ...)]))

;; clojure -> thread first
(define-syntax -->
  (syntax-rules ()
    [(_ m (f fargs ...) args ...) (--> (f m fargs ...) args ...)]
    [(_ m f args ...) (--> (f m) args ...)]
    [(_ arg) arg]))

;; thread last
(define-syntax ->>
  (syntax-rules ()
    [(_ m (f fargs ...) args ...) (->> (f fargs ... m) args ...)]
    [(_ m f args ...) (->> (f m) args ...)]
    [(_ arg) arg]))

(define-syntax as->
  (syntax-rules ()
    [(_ acc bind) acc]
    [(_ acc bind arg args ...) (let1 bind acc (as-> arg bind args ...))]))

;; ---------------------------------------------------------------------------------------------------

(define (equal-one-of? sym . syms) (any (^s (if (equal? sym s) s #f)) syms))

(define (string-prefix? short long)
  (define short-n (string-length short))
  (define long-n (string-length long))
  (and (<= short-n long-n) (string=? short (substring long 0 short-n))))

;; make procedure that counts first characters whose count? is #t
(define ((count-leading-pred count?) s)
  (define n (string-length s))
  (let loop ([at 0])
    (if (or (<= n at) ($ not $ count? $ string-ref s at))
      at
      (loop (+ 1 at)))))

;; convenience
(define (newline? c) (equal? #\newline c))

(define (string-empty? s) (= 0 (string-length s)))

;; ---------------------------------------------------------------------------------------------------
;; custom input port to merge multiple input ports
;; like the one in Racket https://docs.racket-lang.org/reference/port-lib.html#%28def._%28%28lib._racket%2Fport..rkt%29._input-port-append%29%29

(define (eof-reached? in) (eof-object? (peek-char in)))

(define (input-port-append . input-ports)
  (define ins input-ports)
  
  (define (get-input-port!)
    (cond [(null? ins)
           #f]
          [(eof-reached? (car ins))
           (close-input-port (pop! ins))
           (get-input-port!)]
          [else
           (car ins)]))
  
  (make <virtual-input-port>           ; only methods that I recognize
    :getb (^() (and-let1 in (get-input-port!)
                 (read-byte in)))
    :getc (^() (and-let1 in (get-input-port!)
                 (read-char in)))
    :ready (^b (if-let1 in (get-input-port!)
                 (if b (char-ready? in) (byte-ready? in))
                 #t))
    :close (^() (for-each close-input-port ins))))

(define (unget-line line in)
  (input-port-append (open-input-string #"~|line|\n") in))

;; ---------------------------------------------------------------------------------------------------

(define (hash-table/alist-union! a b)
  (define-values (k v) (hash-table-entries b))
  (for-each (^ (k v)
              (cond [(equal-one-of? k 'user 'config 'custom-tag 'special)
                     (hash-table-update! a k (cut alist-merge (^ (a b) a) <> v) '())]
                    [(not (hash-table-contains? a k))
                     (hash-table-set! a k v)]))
            k
            v))
