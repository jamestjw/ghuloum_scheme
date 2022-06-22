(import (test_driver))

(test-case 5 "5" "Runtime - Simple Fixnum")
(test-case #\a "a" "Runtime - Simple Char (a)")
(test-case #\A "A" "Runtime - Simple Char (A)")
(test-case #t "1" "Runtime - Simple Boolean (#t)")
(test-case #f "0" "Runtime - Simple Boolean (#f)")
(test-case '(add1 5) "6" "Unary Primitive - add1")
(test-case '(integer->char 65) "A" "Unary Primitive - integer->char (65 -> A)")
(test-case '(integer->char 100) "d" "Unary Primitive - integer->char (100 -> d)")
(test-case '(char->integer #\a) "97" "Unary Primitive - char->integer (a -> 97)")
(test-case '(char->integer #\d) "100" "Unary Primitive - char->integer (d -> 100)")
