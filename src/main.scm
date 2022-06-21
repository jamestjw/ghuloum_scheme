(define out-file (open-output-file "output.s" 'truncate))

(define (emit . args)
  (apply fprintf out-file args)
  (newline out-file))

(define (compile-program x)
 (emit "\t.text")
 (emit ".globl _scheme_entry")
 ; Not needed by Mach-O assembler
 ; (emit "\t.type scheme_entry, @function")
 (emit "_scheme_entry:")
 (emit "\tmovl $~a, %eax" x)
 (emit "\tret"))

(compile-program 5)
