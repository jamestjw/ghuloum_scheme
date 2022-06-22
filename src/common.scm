(library (common)
  (export contains?)
  (import (rnrs))  
  (define (contains? list x)
    (cond [(null? list) #f]
  	[(equal? (car list) x) #t]
  	[else (contains? (cdr list) x)])))
