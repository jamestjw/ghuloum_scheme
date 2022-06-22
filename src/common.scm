(library (common)
  (export contains? sexpr->string)
  (import (rnrs))

  (define (contains? list x)
    (cond [(null? list) #f]
     [(equal? (car list) x) #t]
     [else (contains? (cdr list) x)]))

  (define join
    (lambda (l delim to-str)
      (fold-left
        (lambda (str elem)
          (string-append str delim (to-str elem)))
        (to-str (car l))
        (cdr l))))

  (define sexpr->string
    (lambda (sexpr)
      (cond
        ((number? sexpr) (number->string sexpr))
        ((symbol? sexpr) (symbol->string sexpr))
        ((boolean? sexpr) (if sexpr "#t" "#f"))
        ((string? sexpr) (string-append "\"" sexpr "\""))
        ((char? sexpr) (string-append "#\\" (string sexpr)))
        ((vector? sexpr)
        (let ((s-vec (join (vector->list sexpr) " " sexpr->string)))
          (string-append "#(" s-vec ")")))
        ((null? sexpr) "()")
        ((list? sexpr) (string-append "(" (join sexpr " " sexpr->string) ")"))

        ((pair? sexpr)
        (let ((s-car (sexpr->string (car sexpr)))
              (s-cdr (sexpr->string (cdr sexpr))))
          (string-append "(" s-car " . " s-cdr ")")))))))
