(library (compiler)
  (export compile-program)
  (import (except (chezscheme) compile-program) (common))  

  (define fixnum-shift 2)
  (define char-shift 8)
  (define fixnum2char-shift (- char-shift fixnum-shift))
  (define char2fixnum-shift fixnum2char-shift)
  (define char-tag (string->number "00001111" 2))
  (define bool-shift 7)
  (define bool-tag (string->number "0011111" 2))
  (define empty-list-value (string->number "00101111" 2))

  (define (out-file) (open-output-file "output.s" 'truncate))

  (define (emit out-port . args)
    (apply fprintf out-port args)
    (newline out-port))

  (define (emit-preamble out-port)
    (emit out-port "\t.text")
    (emit out-port ".globl _scheme_entry")
    ; Not needed by Mach-O assembler
    ; (emit out-port "\t.type scheme_entry, @function")
    (emit out-port "_scheme_entry:"))

  (define (immediate? x)
    (or (integer? x) (char? x) (boolean? x) (eq? x '())))

  (define (immediate-rep x)
    (cond
      ((integer? x) (ash x fixnum-shift))
      ((char? x) (logior (ash (char->integer x) char-shift) char-tag))
      ((boolean? x) (logior (ash (if x 1 0) bool-shift) bool-tag))
      ((eq? x '()) empty-list-value)
      (else (error "compile-program" "Unexpected value type"))))

  ; Assuming that `x` is a list, checks if the quoted expression
  ; is a primitive call, e.g. (primcall? '(add1 2)) -> #t
  (define (primcall? x)
    (contains? '(add1 integer->char char->integer) (car x)))

  ; (define (primcall-op x)
  ;   (case (car x)
  ;     ; TODO: Make this more DRY (consider returning just (car x)?)
  ;     ((add1) 'add1)
  ;     (else (error "primcall-op" "Invalid primitive operator"))))
  
  ; Assume that (primcall? x) -> #true
  (define (primcall-op x)
    (car x))

  ; First argument of a primitive call
  (define (primcall-operand1 x) (cadr x))

  (define (emit-expr out-port x)
    (cond
      [(immediate? x)
          (emit out-port "\tmovl $~a, %eax" (immediate-rep x))]
      [(primcall? x)
        (case (primcall-op x)
          [(add1)
            (emit-expr out-port (primcall-operand1 x))
            (emit out-port "addl $~a, %eax" (immediate-rep 1))]
          [(integer->char)
            (emit-expr out-port (primcall-operand1 x))
            (emit out-port "shl $~a, %eax" fixnum2char-shift)
            (emit out-port "or $~a, %eax" char-tag)]
          [(char->integer)
            (emit-expr out-port (primcall-operand1 x))
            (emit out-port "shr $~a, %eax" char2fixnum-shift)]
          [else (error "emit-expr" "Unknown primitive operator")])]
      [else (error "emit-expr" "Unknown expression encountered")]))

  (define (compile-program x)
   (define of (out-file))
 
   (emit-preamble of)
   (emit-expr of x)
   (emit of "\tret")
 
   (flush-output-port of)
   (close-port of)))
