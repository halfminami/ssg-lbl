(use gauche.test)

(use lib.util)

(test-start "lib.util")

(test-section "input-port-append")

(let1 in (apply input-port-append (map open-input-string '("" "h" "" "e" "" "l" "" "l")))
  (test* "read-byte" (char->integer #\h) (read-byte in))
  (test* "read-char" #\e (read-char in))
  (test* "read-string" "ll" (read-string 10 in))
  (test* "all read" (eof-object) (read-line in))
  (close-input-port in))

(let1 in (apply input-port-append (map open-input-string '("hi" " " "mom\n" "hello")))
  (test* "concat input ports" "hi mom" (read-line in))
  (test* "concat input ports" "hello" (read-line in))
  (test* "all read" (eof-object) (read-line in))
  (close-input-port in))

(let* ([in1 (open-input-string "hello1")]
       [in2 (open-input-string "hello2\n")]
       [in3 (open-input-string "hello3\n")]
       [in (input-port-append in1 in2 in3)])
  (test* "concat input ports" "hello1hello2" (read-line in))
  
  (test* "ports closed after read" (test-truthy) (port-closed? in1))
  (test* "ports not closed" (test-truthy) (not (port-closed? in2)))
  (test* "ports not closed" (test-truthy) (not (port-closed? in3)))
  
  (test* "more input" #\h (peek-char in))
  (close-input-port in)
  
  (test* "ports closed after close" (test-truthy) (port-closed? in2))
  (test* "ports closed after close" (test-truthy) (port-closed? in3)))

(test-end :exit-on-failure #t)
