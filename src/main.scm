(define out-file (open-output-file "output.s" 'truncate))

(define (emit . args)
  (apply fprintf out-file args)
  (newline out-file))

(define fixnum-shift 2)
(define char-shift 8)
(define char-tag (string->number "00001111" 2))
(define bool-shift 7)
(define bool-tag (string->number "0011111" 2))
(define empty-list-value (string->number "00101111" 2))

(define (compile-program x)
  (define (immediate-rep x)
    (cond
      ((integer? x) (ash x fixnum-shift))
      ((char? x) (logior (ash (char->integer x) char-shift) char-tag))
      ((boolean? x) (logior (ash (if x 1 0) bool-shift) bool-tag))
      ((eq? x '()) empty-list-value)
      (#t (error "compile-program" "Unexpected value type"))
      ))
  (emit "\t.text")
  (emit ".globl _scheme_entry")
  ; Not needed by Mach-O assembler
  ; (emit "\t.type scheme_entry, @function")
  (emit "_scheme_entry:")
  (emit "\tmovl $~a, %eax" (immediate-rep x))
  (emit "\tret"))

(compile-program 5)
