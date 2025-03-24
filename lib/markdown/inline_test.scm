(use gauche.test)

(use lib.markdown)
(import lib.markdown.inline-all)

(test-start "lib.markdown.inline")

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

(test-end :exit-on-failure #t)
