(library (compiler)
  (export compile-program)
  (import (except (chezscheme) compile-program) (common))  

  (define fixnum-shift 2)
  (define fixnum-tag 0)
  (define fixnum-tag-mask (string->number "11" 2)) ; Two bits (based on the shift)
  (define char-shift 8)
  (define fixnum2char-shift (- char-shift fixnum-shift))
  (define char2fixnum-shift fixnum2char-shift)
  (define char-tag (string->number "00001111" 2))
  (define char-tag-mask (string->number "11111111" 2)) ; Eight bits (based on the shift)
  (define bool-shift 7)
  (define bool-tag (string->number "0011111" 2))
  (define bool-tag-mask (string->number "1111111" 2)) ; Seven bits (based on the shift)
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

  ; (define (primcall-op x)
  ;   (case (car x)
  ;     ; TODO: Make this more DRY (consider returning just (car x)?)
  ;     ((add1) 'add1)
  ;     (else (error "primcall-op" "Invalid primitive operator"))))

  ; Make it more convenient to define a primitive
  ; Binds the argument count and the code to emit
  ; the function call to a symbol corresponding
  ; to the name of the primitive
  (define-syntax define-primitive
    (syntax-rules ()
     [(_ (prim-name out-port arg* ...) b b* ...)
      (begin
        (putprop 'prim-name '*is-prim* #t)
        (putprop 'prim-name '*arg-count* (length '(arg* ...)))
        (putprop 'prim-name '*emitter* (lambda (out-port arg* ...) b b* ...)))]))

  (define (primitive? x)
    (and (symbol? x) (getprop x '*is-prim*)))

  (define (primitive-emitter x)
    (or (getprop x '*emitter*) (error "primitive-emitter" "Invalid primitive")))

  ; Verifies that `x` is a list, and then checks that
  ; the first symbol corresponds to a primitive
  (define (primcall? x)
    (and (pair? x) (primitive? (car x))))

  (define (check-primcall-args prim args)
    (if (= (length args) (getprop prim '*arg-count*))
      '() ; Do nothing if the lengths match
      (error "check-primcall-args" (string-append "Invalid number of arguments for " (symbol->string prim)))))

  (define (emit-primcall output-port expr)
    (let ([prim (car expr)] [args (map eval (cdr expr))])
      (check-primcall-args prim args) ; Verify that args are valid for this primcall
      (apply (primitive-emitter prim) output-port args)))

  (define (emit-expr out-port expr)
    (cond
      [(immediate? expr)
          (emit out-port "\tmovl $~a, %eax" (immediate-rep expr))]
      [(primcall? expr) (emit-primcall out-port expr)]
      [else (error "emit-expr" (string-append "Unknown expression " (sexpr->string expr) " encountered"))]))

  (define (compile-program x)
   (define of (out-file))

   (emit-preamble of)
   (emit-expr of x)
   (emit of "\tret")

   (flush-output-port of)
   (close-port of))

  ; ******* Definition of primitives ******
  (define-primitive (add1 out-port arg)
    (if (integer? arg)
      (begin
        (emit-expr out-port arg)
        (emit out-port "\taddl $~s, %eax" (immediate-rep 1)))
      (error "emit-expr" "add1? can only be called with integers")))

  (define-primitive (sub1 out-port arg)
    (if (integer? arg)
      (begin
        (emit-expr out-port arg)
        (emit out-port "\tsubl $~s, %eax" (immediate-rep 1)))
      (error "emit-expr" "sub1? can only be called with integers")))

  (define-primitive (integer->char out-port arg)
    (emit-expr out-port arg)
    (emit out-port "\tshl $~a, %eax" fixnum2char-shift)
    (emit out-port "\tor $~a, %eax" char-tag))

  (define-primitive (char->integer out-port arg)
    (emit-expr out-port arg)
    (emit out-port "\tshr $~a, %eax" char2fixnum-shift))

  (define-primitive (zero? out-port arg)
    (if (integer? arg)
      (begin
        (emit-expr out-port arg)
        (emit out-port "\tcmpl $0, %eax")              ; Compares %eax to 0
        (emit out-port "\tmovl $0, %eax")              ; Zeroes %eax
        (emit out-port "\tsete %al")                   ; Set lower byte of %eax to 1 if comparison was successful
        (emit out-port "\tsall $~a, %eax" bool-shift)  ; Apply appropriate shift
        (emit out-port "\torl $~a, %eax" bool-tag))    ; and tag
      (error "emit-expr" "zero? can only be called with integers")))

  (define-primitive (null? out-port arg)
    (emit-expr out-port arg)
    (emit out-port "\tcmpl $~a, %eax" empty-list-value) ; Compares %eax to empty-list-value
    (emit out-port "\tmovl $0, %eax")                   ; Zeroes %eax
    (emit out-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
    (emit out-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
    (emit out-port "\torl $~a, %eax" bool-tag))         ; and tag

  (define-primitive (not out-port arg)
     (emit-expr out-port arg)
     (emit out-port "\tcmpl $~a, %eax" bool-tag)         ; Compares %eax to the representation of false
     (emit out-port "\tmovl $0, %eax")                   ; Zeroes %eax
     (emit out-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
     (emit out-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
     (emit out-port "\torl $~a, %eax" bool-tag))         ; and tag

  (define-primitive (integer? out-port arg)
    (emit-expr out-port arg)
    (emit out-port "\tand $~a, %eax" fixnum-tag-mask)     ; Gets the tag of a fixnum
    (emit out-port "\tcmpl $~a, %eax" fixnum-tag)         ; Compares %eax to a tag of a fixnum
    (emit out-port "\tmovl $0, %eax")                   ; Zeroes %eax
    (emit out-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
    (emit out-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
    (emit out-port "\torl $~a, %eax" bool-tag))         ; and tag

  (define-primitive (boolean? out-port arg)
    (emit-expr out-port arg)
    (emit out-port "\tand $~a, %eax" bool-tag-mask)     ; Gets the tag of a boolean
    (emit out-port "\tcmpl $~a, %eax" bool-tag)         ; Compares %eax to bool-tag
    (emit out-port "\tmovl $0, %eax")                   ; Zeroes %eax
    (emit out-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
    (emit out-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
    (emit out-port "\torl $~a, %eax" bool-tag))         ; and tag

  (define-primitive (char? out-port arg)
    (emit-expr out-port arg)
    (emit out-port "\tand $~a, %eax" char-tag-mask)     ; Gets the tag of a char
    (emit out-port "\tcmpl $~a, %eax" char-tag)         ; Compares %eax to char-tag
    (emit out-port "\tmovl $0, %eax")                   ; Zeroes %eax
    (emit out-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
    (emit out-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
    (emit out-port "\torl $~a, %eax" bool-tag))         ; and tag
  ; ******* Definition of primitives ******
)
