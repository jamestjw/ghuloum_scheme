#!chezscheme
(library (color)
  (export println-color)
  (import (chezscheme))

  (define-syntax define-color
    (syntax-rules ()
     [(_ color-name code)
      (begin
        (putprop 'color-name '*code* code))]))

  (define (get-color-code c)
    (or (getprop c '*code*) (error "get-color-code" "Invalid color")))

  (define (println-color color . args)
    (let ([output-string (open-output-string)])
      (apply fprintf output-string args)
      (display (string-append (get-color-code color) " " (get-output-string output-string) (get-color-code 'END-COLOR) "\n"))))

  (define-color HEADER "\033[95m")
  (define-color OKBLUE "\033[94m")
  (define-color OKCYAN "\033[96m")
  (define-color OKGREEN "\033[92m")
  (define-color WARNING "\033[93m")
  (define-color FAIL "\033[91m")
  (define-color ENDC "\033[0m")
  (define-color BOLD "\033[1m")
  (define-color UNDERLINE "\033[4m")
  (define-color END-COLOR "\033[0m"))
