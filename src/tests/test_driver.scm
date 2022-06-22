(library (test_driver)
  (export test-case)
  (import (except (chezscheme) compile-program) (compiler))

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

  (define (test-case e expected name)
    (let ((result (compile-run-exp e)))
        (printf "~a... " name)
    (cond ((string=? result expected) (printf "PASSED\n"))
          (else (printf "FAILED\nResult: ~a, Expected: ~a\n" result expected))))))
