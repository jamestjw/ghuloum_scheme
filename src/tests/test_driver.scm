(library (test_driver)
  (export test-case test-exception test-case-with-expr-appended-to-name)
  (import (except (chezscheme) compile-program) (compiler) (common) (color))

  ; https://stackoverflow.com/questions/71627605/capturing-the-output-of-an-external-call-as-a-string-in-chez-scheme
  (define (capture-standard-output command)
      (let-values ([(in out err pid) (open-process-ports command 'block
                                      (make-transcoder (utf-8-codec)))])
        (get-string-all out)))

  (define (remove-trailing-newline x)
    ; Remove last character of the string
    (substring x 0 (- (string-length x) 1)))

  ; TODO: Improve error handling, i.e.
  ; ensure that the proper cleanup is
  ; not neglected
  (define (compile-run-exp e)
    (compile-program e)
    (system "gcc output.s src/runtime.c -o out")
    (system "rm output.s")
    (let ((res (remove-trailing-newline (capture-standard-output "./out"))))
      (system "rm ./out")
      res))

  (define test-filter (getenv "FILTER"))

  ; Check if a filter is given and skip
  ; tests that don't match it
  (define (execute-test? name)
    (if test-filter
      (string-contains? name test-filter)
      #t)) ; Return true if no filter is given

  (define (test-case-with-expr-appended-to-name e expected name)
    (test-case e expected (string-append name " - " (sexpr->string e))))

  (define (test-case e expected name)
    (if (execute-test? name)
      (let ((result (compile-run-exp e)))
        (printf "~a... " name)
        (cond
          ((string=? result expected) (print-passed))
          (else (begin
            (print-failed)
            (printf "Result: ~a, Expected: ~a\n" result expected)))))
      '()))

  (define (test-exception e name)
    (if (execute-test? name)
      (begin
        (printf "~a (Exception expected)... " name)
        (guard
          (x [else (print-passed)])
          (begin
            (compile-run-exp e)
            (print-failed)))
      )
      '()))

  (define (print-passed) (println-color 'OKGREEN "PASSED"))
  (define (print-failed) (println-color 'FAIL "FAILED")))
