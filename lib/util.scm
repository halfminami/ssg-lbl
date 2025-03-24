(define-module lib.util
  (export thunk
	  
	  -->
	  ->>
	  as->

	  equal-one-of?

	  string-prefix?
	  count-leading-pred
	  newline?
	  string-empty?))

(select-module lib.util)

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
