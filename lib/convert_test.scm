(use gauche.test)

(load "./convert")
(import convert.all)

(test-start "convert")

;; simply comparing by string, not HTML representation
;; more like diff test, subject to change..

(test-section "next-codeblock")

(define (run-next-codeblock str env)
  (define in (open-input-string str))
  (define out (open-output-string))
  
  (next-codeblock (read-line in) in out env)

  (begin0
   `(,(get-output-string out) ,(port->string in))
   (close-input-port in)
   (close-output-port out)))

(define hljs (hash-table-from-pairs 'equal? '(codeblock-class . "hljs")))

(test* "nothing"
       '("<pre><code class=\"\"></code></pre>\n" "\n") (run-next-codeblock "```\n```\n\n" (make-hash-table)))

(test* "normal"
       '("<pre><code class=\"hljs language-js\">const b = 2 &lt; 3;

// your
</code></pre>\n" "mom")
       (run-next-codeblock "```js
const b = 2 < 3;

// your
```
mom"
			    hljs))

(test* "ill"
       '("<pre><code class=\"hljs language-js\">const b = 2 &lt; 3;

// your```mom
</code></pre>\n" "")
       (run-next-codeblock "```js
const b = 2 < 3;

// your```mom"
			   hljs))

#|
(test-section "next-ul!")

(define (run-next-ul str :optional [env (make-hash-table)])
  (define in (open-input-string str))
  (define out (open-output-string))

  (define last-line (next-ul! (read-line in) in out env))
  
  (begin0
   `(,last-line ,(get-output-string out) ,(port->string in))
   (close-input-port in)
   (close-output-port out)))

(test* "- one"
       `(,(undefined) "<ul><li>hello</li>\n</ul>" "") (run-next-ul "- hello"))
(test* "+ one"
       `(,(undefined) "<ul><li>hello</li>\n</ul>" "") (run-next-ul "+ hello"))

(test* "flat"
       '("-not li" "<ul><li>a1</li>\n<li>a2</li>\n<li>a3</li>\n</ul>" "more not li")
       (run-next-ul "- a1\n- a2\n- a3\n-not li\nmore not li"))
(test* "nest open"
       `(,(undefined)
	 "<ul><li>top</li>\n<ul><li>deep</li>\n<li>dep</li>\n<ul><li>deeper</li>\n</ul></ul></ul>"
	 "")
       (run-next-ul "- top\n  - deep\n  - dep\n    - deeper"))
(test* "nest closed"
       `(,(undefined)
	 "<ul><li>top</li>\n<ul><li>deep</li>\n<ul><li>deeper</li>\n</ul><li>deep2</li>\n</ul><li>top1</li>\n</ul>"
	 "")
       (run-next-ul "- top\n  - deep\n    - deeper\n  - deep2\n- top1"))
(test* "nest closed jump"
       `(,(undefined)
	 "<ul><li>top</li>\n<ul><li>deep</li>\n<ul><li>deeper</li>\n</ul></ul><li>top1</li>\n</ul>"
	 "")
       (run-next-ul "- top\n  - deep\n    - deeper\n- top1"))
(test* "nest back and forth"
       `(,(undefined)
	 "<ul><li>top</li>\n<ul><li>deep</li>\n<ul><li>deeper</li>\n</ul><li>top1</li>\n<ul><li>deeper again</li>\n</ul></ul><li>simp</li>\n<ul><li>no</li>\n</ul></ul>"
	 "")
       (run-next-ul "- top\n  - deep\n    - deeper\n  - top1\n    - deeper again\n- simp\n  - no"))

(test* "close newline"
       '("" "<ul><li>top</li>\n<ul><li>deep</li>\n</ul></ul>" "- bye")
       (run-next-ul "- top\n  - deep\n\n- bye"))
(test* "newline continue"
       `(,(undefined) "<ul><li>top\ncontinue</li>\n<li>another\ncontinue</li>\n</ul>" "")
       (run-next-ul "- top\n  continue\n- another\n  continue"))
(test* "newline continue"
       `(,(undefined) "<ul><li>top</li>\n<ul><li>deep\ncontinue</li>\n</ul><li>top2\ncontinue</li>\n</ul>" "")
       (run-next-ul "- top\n  - deep\n    continue\n- top2\n  continue"))

(test-section "next-ol!")
;; short test since code is almost the same as ul version

(define (run-next-ol str :optional [env (make-hash-table)])
  (define in (open-input-string str))
  (define out (open-output-string))

  (define last-line (next-ol! (read-line in) in out env))
  
  (begin0
   `(,last-line ,(get-output-string out) ,(port->string in))
   (close-input-port in)
   (close-output-port out)))

(test* "nest"
       `(,(undefined) "<ol><li>324</li>\n<li>hell</li>\n<ol><li>boo</li>\n</ol></ol>" "")
       (run-next-ol "1. 324\n22372. hell\n  281. boo"))
|#
(test-section "markdown->html! w/o inline")

(define (run-markdown->html str :optional [env (make-hash-table)])
  (define in (open-input-string str))
  (define out (open-output-string))

  (markdown->html! in out env)
  (begin0
   (get-output-string out)
   (close-input-port in)
   (close-output-port out)))

;; I could use list of strings, but the last newline was significant..
(test* "flat"
       "\
<p>hello</p>
<h1>HELLO</h1>
<ul><li>l1</li>
</ul>
<ol><li>l2</li>
</ol>
<hr>"
       (run-markdown->html "\
hello
# HELLO
- l1
1. l2
---")
       test-check-diff test-report-failure-diff)

(test* "nest blockquote"
       "\
<blockquote><h3>hello</h3>
<blockquote><p>hi</p>
</blockquote>
<p>whassup</p>
<blockquote><p>nothin</p>
</blockquote>
</blockquote>
"
       (run-markdown->html "\
>### hello
>>hi
>whassup
>>nothin")
       test-check-diff test-report-failure-diff)

(test* "nest list"
       "\
<ul><li>top</li>
<ul><li>middle</li>
<ul><li>bottom</li>
</ul>
<li>middle 2</li>
<li>middle 3</li>
</ul>
<li>top 2</li>
<ul><li>middle 3</li>
</ul>
<ol><li>numbered</li>
</ol>
</ul>
<ol><li>numbered 2</li>
<li>numbered 3</li>
<ol><li>numbered 4</li>
</ol>
<li>NUMBER</li>
</ol>
"
       (run-markdown->html "\
- top
  - middle
    - bottom
  - middle 2
  - middle 3
- top 2
  - middle 3
  1. numbered
1. numbered 2
1. numbered 3
  1. numbered 4
04932. NUMBER")
       test-check-diff test-report-failure-diff)

(test* "nest ul list"
       "\
<ul><li>hello 1</li>
</ul>
<ul><li>hello 2</li>
<ul><li>hi 1</li>
<li>hi 2</li>
<li>hi 3</li>
<ul><li>h 1</li>
<li>h 2</li>
</ul>
</ul>
<li>hello 3</li>
<ul><li>hi 4</li>
</ul>
<li>hello 4</li>
</ul>
"
       (run-markdown->html "\
- hello 1

+ hello 2
  * hi 1
  + hi 2
  - hi 3
    * h 1
    * h 2
- hello 3
  - hi 4
- hello 4")
       test-check-diff test-report-failure-diff)

(test* "nest ol list"
       "\
<ol><li>hello 1</li>
</ol>
<ol><li>hello 2</li>
<ol><li>hi 1</li>
<li>hi 2</li>
<li>hi 3</li>
<ol><li>h 1</li>
<li>h 2</li>
</ol>
</ol>
<li>hello 3</li>
<ol><li>hi 4</li>
</ol>
<li>hello 4</li>
</ol>
"
       (run-markdown->html "\
1. hello 1

1. hello 2
  1. hi 1
  1. hi 2
  1. hi 3
    1. h 1
    1. h 2
1. hello 3
  1. hi 4
1. hello 4")
       test-check-diff test-report-failure-diff)

(test* "flat ul"
       "\
<ul><li>hello 1</li>
<li>hello 2</li>
</ul>
<ul><li>hello 3</li>
</ul>
<p>what</p>
"
       (run-markdown->html "\
- hello 1
- hello 2

- hello 3
what
")
       test-check-diff test-report-failure-diff)

(test* "nest blockquote"
       "\
<blockquote><p>ok</p>
<ul><li>hello</li>
<ul><li>ok</li>
</ul>
</ul>
<blockquote><p>what</p>
<ul><li>na</li>
<ul><li>mama</li>
</ul>
</ul>
<p>TODO: allow blank line</p>
</blockquote>
<h1> hey!</h1>
</blockquote>
"
       (run-markdown->html "\
>ok
>- hello
>  - ok
>>what
>>- na
>>  - mama
>>TODO: allow blank line
>#  hey!")
       test-check-diff test-report-failure-diff)

(test* "inside blockquote"
       "\
<blockquote><p>hello</p>
<h1>HELLO</h1>
<ul><li>l1</li>
</ul>
<ol><li>l2</li>
</ol>
<pre><code class=\"language-js\">// who's gonna code inside quote

// sad
</code></pre>
</blockquote>
"
       (run-markdown->html "\
>hello
># HELLO
>- l1
>1. l2
>```js
>// who's gonna code inside quote
>
>// sad
>```")
       test-check-diff test-report-failure-diff)

(test* "para"
       "\
<p>hello
hell</p>
<p>hi
bye</p>
"
       (run-markdown->html "\
hello
hell

hi
bye")
       test-check-diff test-report-failure-diff)

(test* "parser off"
       "\
<ul><li>hello<div>I AM ALIVE</div>
</li>
<li>did I see anything</li>
</ul>
"
       (run-markdown->html "\
- hello
;;;off
<div>I AM ALIVE</div>
;;;on
- did I see anything")
       test-check-diff test-report-failure-diff)

(test-section "branch-inline!")

(define (run-branch-inline str :optional [env (make-hash-table)])
  (define out (open-output-string))

  (branch-inline! str out env)
  
  (begin0
   (get-output-string out)
   (close-output-port out)))

(test* "* flat"
       "hello <em>HTML</em> and <strong>CSS</strong>"
       (run-branch-inline "hello *HTML* and **CSS**")
       test-check-diff test-report-failure-diff)

(test* "_ flat"
       "hello <em>HTML</em> and <strong>CSS</strong>"
       (run-branch-inline "hello _HTML_ and __CSS__")
       test-check-diff test-report-failure-diff)

(test* "~~ and ` flat"
       "I <s>dis</s>like <code>Python</code>"
       (run-branch-inline "I ~~dis~~like `Python`")
       test-check-diff test-report-failure-diff)

(test* "` escape"
       "you can use <code>&grave;</code> to format <code>&lt;code&gt;</code>"
       (run-branch-inline "you can use `&grave;` to format `<code>`")
       test-check-diff test-report-failure-diff)

(test* "` no nest"
       "<code>**hello**</code> will format <strong>hello</strong>"
       (run-branch-inline "`**hello**` will format **hello**")
       test-check-diff test-report-failure-diff)

(test* "nest"
       "<strong><em>too many?</em></strong> <em><s>always</s> <strong>never</strong> gonna give <em>you</em> up</em>"
       (run-branch-inline "**_too many?_** _~~always~~ **never** gonna give *you* up_")
       test-check-diff test-report-failure-diff)

(test-section "markdown->html! w/ inline")

(test* "don't choke on invalid input; many *_"
       (test-truthy)
       (run-markdown->html "\
hell*o

you **_*
__are _*
**_* * _
 * _ amanda
> "))

(test* "don't choke on invalid input; list and h1"
       (test-truthy)
       (run-markdown->html "\
# hello *world
mah*

- he~~llo *hello
- hell~~o ** hello
  - ** h~~ell~~o*
  - hello*
>- he**llo
hello**
----"))

(test* "long"
       "\
<h1>hello!</h1>
<p>I am <em>building a <strong>
blog</strong></em>!
<s>I'm not sure if <strong>I
should</strong> do it, though.</s></p>
<hr><p>this:</p>
<ul><li>is a parser <strong>from scratch</strong></li>
<li>reads line by line</li>
<ul><li>less feature</li>
</ul>
</ul>
<blockquote><p>I <em>guess</em> it's <em><strong>not
efficient</strong></em>.</p>
</blockquote>
"
       (run-markdown->html "\
# hello!

I am *building a __
blog__*!
~~I'm not sure if **I
should** do it, though.~~

---

this:
- is a parser __from scratch__
- reads line by line
  - less feature
>I *guess* it's _**not
>efficient**_.")
       test-check-diff test-report-failure-diff)

(test* "inline"
       "\
<h1>Looking at <em>You</em></h1>
<ul><li>hello <em>1
hello</em> 2
hello 3</li>
<ul><li>hello <strong>4
hello 5
hello</strong> 6</li>
</ul>
</ul>
"
       (run-markdown->html "\
# Looking at *You*

- hello *1
  hello* 2
  hello 3
  - hello __4
    hello 5
    hello__ 6
")
       test-check-diff test-report-failure-diff)

(test-section "markdown->html! w/ variables")

(test* "flat"
       "\
<h1>My <marquee>blog</marquee></h1>
<p>created at 2025-03-22</p>
<ul><li>hello me!</li>
</ul>
"
       (run-markdown->html "\
# {title}

created at {created}
- hello {name}!"
			   (hash-table-from-pairs 'eq?
						  '(user . (("title" . "My <marquee>blog</marquee>") ("created" . "2025-03-22") ("name" . "me")))))
       test-check-diff test-report-failure-diff)

(test* "newline is space"
       "<p>\nHELLO</p>\n"
       (run-markdown->html "\
{hello long
name}"
			   (hash-table-from-pairs 'eq?
						  '(user . (("hello long name" . "HELLO")))))
       test-check-diff test-report-failure-diff)

(test* "don't choke on invalid input; non-existent and runaway"
       (test-truthy)
       (run-markdown->html "\
}}}{{{}}}
{ja?

}

- {run
- away}

{run away}"
			   (hash-table-from-pairs 'eq?
						  '(user . ()))))

(test-end :exit-on-failure #t)
