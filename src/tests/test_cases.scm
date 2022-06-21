(load "test_driver.scm")

(test-case 5 "5" "Runtime - Simple Fixnum")
(test-case #\a "a" "Runtime - Simple Char (a)")
(test-case #\A "A" "Runtime - Simple Char (A)")
(test-case #t "1" "Runtime - Simple Boolean (#t)")
(test-case #f "0" "Runtime - Simple Boolean (#f)")

