;; test for markdown->html!
;; I will add tests as I use this program

(use gauche.test)

(use lib.markdown)
(import lib.markdown-all)

(test-start "lib.markdown")

;; simply comparing by string, not HTML representation
;; more like diff test, subject to change..

(test-section "next-codeblock")

(define (run-next-codeblock str env)
  (define in (open-input-string str))
  (define out (open-output-string))

  (next-codeblock (read-line in) in out env)

  (begin0 `(,(get-output-string out) ,(port->string in))
    (close-input-port in)
    (close-output-port out)))

(define hljs
  (hash-table-from-pairs 'eq?
			 '(custom-tag . ((codeblock . ((classname . "hljs")))))))

(test* "nothing"
       '("<pre><code></code></pre>\n" "\n") (run-next-codeblock "```\n```\n\n" (make-hash-table)))

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

(test-section "markdown->html! w/o inline")

(define (run-markdown->html str :optional [env (make-hash-table)])
  (define in (open-input-string str))
  (define out (open-output-string))

  (markdown->html! in out env)
  (begin0 (get-output-string out)
    (close-input-port in)
    (close-output-port out)))

;; I could use list of strings, but the last newline was significant..
(test* "flat"
       "\
<p>hello</p>
<h1 id=\"hello\">HELLO</h1>
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
<blockquote><h3 id=\"hello\">hello</h3>
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

(test* "lower list from ol to ul"
       "\
<ol><li>hello</li>
<ol><li>hey</li>
</ol>
</ol>
<ul><li>duh</li>
</ul>
"
       (run-markdown->html "\
1. hello
  1. hey
- duh")
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

(test* "lists combined"
       "\
<ul><li>hello 1</li>
<ul><li>hi 1</li>
<ol><li>hell 1</li>
</ol>
</ul>
</ul>
<ol><li>hello 2</li>
<ul><li>hi 2</li>
</ul>
<li>hello 3</li>
<ul><li>hi 3</li>
</ul>
<ol><li>hi 4</li>
<ul><li>hell 2</li>
</ul>
</ol>
</ol>
"
       (run-markdown->html "\
- hello 1
  - hi 1
    1. hell 1
1. hello 2
  - hi 2
1. hello 3
  - hi 3
  1. hi 4
    - hell 2")
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
<h1 id=\"-hey!\"> hey!</h1>
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
<h1 id=\"hello\">HELLO</h1>
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
<h1 id=\"hello!\">hello!</h1>
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
<h1 id=\"looking-at-*you*\">Looking at <em>You</em></h1>
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
<p>My <marquee>blog</marquee></p>
<p>created at 2025-03-22</p>
<ul><li>hello me!</li>
</ul>
"
       (run-markdown->html "\
{title}

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

(test-section "markdown->html! w/ links")

(test* "a simple"
       "<p>hello <a href=\"wikipedia.org\">Wikipedia</a>.</p>\n"
       (run-markdown->html "hello [Wikipedia](wikipedia.org).")
       test-check-diff test-report-failure-diff)

(test* "a multiline"
       "\
<p>
<a href=\"https://example.com/\">really
long</a></p>
"
       (run-markdown->html "\
[really
long](https://example.com/)")
       test-check-diff test-report-failure-diff)

(test* "a everywhere"
       "\
<ul><li>check out <a href=\"https://example.com/\">the website</a></li>
</ul>
<blockquote><p>check out <a href=\"https://example.com/\">the website</a></p>
</blockquote>
<p>check out <a href=\"https://example.com/\">the <strong>website</strong></a><br>
<a href=\"https://docs.example.com/\"><code>printf</code></a></p>
"
       (run-markdown->html "\
- check out [the website](https://example.com/)

>check out [the website](https://example.com/)

check out [the **website**](https://example.com/)<br>
[`printf`](https://docs.example.com/)")
       test-check-diff test-report-failure-diff)

(test* "a fake"
       "\
<p>[this <em>and</em>] [this is not a link]
</p>
<p>[this too is not] (a link)</p>
"
       (run-markdown->html "\
[this *and*] [this is not a link]

[this too is not] (a link)")
       test-check-diff test-report-failure-diff)

(test* "don't choke on invalid input"
       (test-truthy)
       (run-markdown->html "[hello hello\nhello\n\n[hello](hello hello\n"))

(test-section "custom-tag rule")

(test* "blockquote"
       "\
<div class=\"left-indent dim\"><p>feels like blockquotes are abused
in markdown. they look pretty but
they aren't actual quotes most of the time.</p>
</div>
"
       (run-markdown->html "\
>feels like blockquotes are abused
>in markdown. they look pretty but
>they aren't actual quotes most of the time."
			   (alist->hash-table '((custom-tag . ((blockquote . ((tagname . "div") (classname . "left-indent dim"))))))))
       test-check-diff test-report-failure-diff)

(test* "ul and ol"
       "\
<ul class=\"no-bullet\" contenteditable><li class=\"list\">hello?</li>
<ul class=\"no-bullet\" contenteditable><li class=\"list\">ok?</li>
</ul>
</ul>
<ol class=\"roman\"><li class=\"list\">hi?</li>
</ol>
"
       (run-markdown->html "\
- hello?
  - ok?
1. hi?"
			   (alist->hash-table '((custom-tag . ((ul . ((classname . "no-bullet") (raw . "contenteditable")))
							       (li . ((classname . "list")))
							       (ol . ((classname . "roman"))))))))
       test-check-diff test-report-failure-diff)

(test* "h1, h3 and p"
       "\
<p class=\"lead\" id=\"don't-use-many-h1s!\">don't use many h1s!</p>
<h2 id=\"h2-here\">h2 here</h2>
<h3 class=\"big dim\" id=\"h3-here\">h3 here</h3>
<p style=\"text-align: center; font-size: 2em\">and p</p>
"
       (run-markdown->html "\
# don't use many h1s!
## h2 here
### h3 here
and p"
			   (alist->hash-table '((custom-tag . ((h1 . ((tagname . "p") (classname . "lead")))
							       (h3 . ((classname . "big dim")))
							       (p . ((raw . "style=\"text-align: center; font-size: 2em\""))))))))
       test-check-diff test-report-failure-diff)

(test* "strong, em, s, code"
       "\
<p><b class=\"important\">strong</b>
<i class=\"term\" data-tooltip=\"hover\">em</i>
<samp>code</samp></p>
"
       (run-markdown->html "\
**strong**
*em*
`code`"
			   (alist->hash-table '((custom-tag . ((strong . ((tagname . "b") (classname . "important")))
							       (em . ((tagname . "i") (classname . "term") (raw . "data-tooltip=\"hover\"")))
							       (code . ((tagname . "samp"))))))))
       test-check-diff test-report-failure-diff)

(test* "hr"
       "<hr data-content=\"true\">"
       (run-markdown->html "---"
			   (alist->hash-table '((custom-tag . ((hr . ((raw . "data-content=\"true\""))))))))
       test-check-diff test-report-failure-diff)

(test* "a"
       "<p><a class=\"new-tab\" target=\"_blank\" href=\"https://example.com/survey/2020\">take the survey</a></p>\n"
       (run-markdown->html "[take the survey](https://example.com/survey/2020)"
			   (alist->hash-table '((custom-tag . ((a . ((classname . "new-tab") (raw . "target=\"_blank\""))))))))
       test-check-diff test-report-failure-diff)

(test-section "table of contents")

(define (headings-markdown->html str :optional [env (make-hash-table)])
  (define in (open-input-string str))
  (define out (open-output-string))

  (begin0 (markdown->html! in out env)
    (close-input-port in)
    (close-output-port out)))

(test* "single h1"
       '((h1 . "hello"))
       (headings-markdown->html "\
# hello"))

(test* "many"
       '((h1 . "simple-ssg-from-scratch")
         (h2 . "directory-structure")
         (h2 . "run")
         (h3 . "uses")
         (h2 . "test"))
       (headings-markdown->html "\
# Simple SSG from scratch
## Directory Structure
- TODO

## Run
### Uses
## Test"))

(test-end :exit-on-failure #t)
