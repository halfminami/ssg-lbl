(use gauche.test)

(use lib.html)
(import lib.html-all)

(test-start "lib.html")

(test-section "html->html")

(define (run-html->html str :optional [env (make-hash-table)])
  (call-with-string-io str
    (^(in out) (html->html in out env))))

(test*/diff "many"
            "<p>this is\n<strong>not a drill</strong></p>"
            (run-html->html "<p>{{what}} {{be}}\n<strong>not a {{thing}}</strong></p>"
                            (hash-table-from-pairs 'eq?
                                                   '(user . (("what" . "this") ("be" . "is") ("thing" . "drill"))))))

(test*/diff "newline is space"
            "<p>HELLO</p>"
            (run-html->html "\
<p>{{hello long
name}}</p>"
		            (hash-table-from-pairs 'eq?
		       		                   '(user . (("hello long name" . "HELLO"))))))

(test-end :exit-on-failure #t)
