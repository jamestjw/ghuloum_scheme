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

  (define word-size 8)

  (define (out-file) (open-output-file "output.s" 'truncate))

  (define (emit out-port . args)
    (apply fprintf out-port args)
    (newline out-port))

  (define (emit-function-header out-port fn-name)
    (emit out-port "\t.text")
    (emit out-port (string-append ".globl " fn-name))
    ; Not needed by Mach-O assembler
    ; (emit out-port "\t.type scheme_entry, @function")
    (emit out-port (string-append fn-name ":")))

  ; Does nothing for now
  (define (emit-preamble out-port) '())

  (define (immediate? x)
    (or (integer? x) (char? x) (boolean? x) (null? x)))

  (define (immediate-rep x)
    (cond
      ((integer? x) (ash x fixnum-shift))
      ((char? x) (logior (ash (char->integer x) char-shift) char-tag))
      ((boolean? x) (logior (ash (if x 1 0) bool-shift) bool-tag))
      ((null? x) empty-list-value)
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
     [(_ (prim-name out-port si arg* ...) b b* ...)
      (begin
        (putprop 'prim-name '*is-prim* #t)
        (putprop 'prim-name '*arg-count* (length '(arg* ...)))
        (putprop 'prim-name '*emitter* (lambda (out-port si arg* ...) b b* ...)))]))

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

  (define (emit-primcall output-port si expr)
    (let ([prim (car expr)] [args (cdr expr)])
      (check-primcall-args prim args) ; Verify that args are valid for this primcall
      (apply (primitive-emitter prim) output-port si args)))

  (define (emit-immediate out-port x)
    (emit out-port "\tmovl $~a, %eax" (immediate-rep x)))

  (define (emit-expr out-port si expr)
    (cond
      [(immediate? expr) (emit-immediate out-port expr)]
      [(primcall? expr) (emit-primcall out-port si expr)]
      [(eq? '() (eval expr)) (emit-expr out-port si '())] ; Workaround until we can deal with quotes
      [else (error "emit-expr" (string-append "Unknown expression " (sexpr->string expr) " encountered"))]))

  (define (compile-program expr)
    (define of (out-file))

    (emit-preamble of)

    (emit-function-header of "_L_scheme_entry")
    (emit-expr of (- word-size) expr)
    (emit of "\tret")

    (emit-function-header of "_scheme_entry")
    (emit of "\tmovq %rsp, %rbx") ; Save current value of rsp
    (emit of "\tmovq %rdi, %rsp") ; Assign new value of rsp (from 1st arg of _scheme_entry)
    (emit of "\tcall _L_scheme_entry")
    (emit of "\tmovq %rbx, %rsp")
    (emit of "\tret")

    (flush-output-port of)
    (close-port of))

  (define (emit-binary-comparison op out-port si arg1 arg2)
    (define asm_op (case op
      ((=) "sete")
      ((>) "setg")
      ((>=) "setge")
      ((<) "setl")
      ((<=) "setle")
      (else (error "emit-binary-comparison" "Invalid operator"))))
    (emit-expr out-port si arg1)
    (emit out-port "\tmovl %eax, ~s(%rsp)" si)
    (emit-expr out-port (- si word-size) arg2)
    (emit out-port "\tcmpl %eax, ~s(%rsp)" si)
    (emit out-port "\tmovl $0, %eax")
    (emit out-port "\t~s %al" asm_op)
    (emit out-port "\tsall $~a, %eax" bool-shift)
    (emit out-port "\torl $~a, %eax" bool-tag))

  ; ******* Definition of primitives ******
  (define-primitive (add1 out-port si arg)
    (if (integer? arg)
      (begin
        (emit-expr out-port si arg)
        (emit out-port "\taddl $~s, %eax" (immediate-rep 1)))
      (error "emit-expr" "add1? can only be called with integers")))

  (define-primitive (sub1 out-port si arg)
    (if (integer? arg)
      (begin
        (emit-expr out-port si arg)
        (emit out-port "\tsubl $~s, %eax" (immediate-rep 1)))
      (error "emit-expr" "sub1? can only be called with integers")))

  (define-primitive (integer->char out-port si arg)
    (emit-expr out-port si arg)
    (emit out-port "\tshl $~a, %eax" fixnum2char-shift)
    (emit out-port "\tor $~a, %eax" char-tag))

  (define-primitive (char->integer out-port si arg)
    (emit-expr out-port si arg)
    (emit out-port "\tshr $~a, %eax" char2fixnum-shift))

  (define-primitive (zero? out-port si arg)
    (if (integer? arg)
      (begin
        (emit-expr out-port si arg)
        (emit out-port "\tcmpl $0, %eax")              ; Compares %eax to 0
        (emit out-port "\tmovl $0, %eax")              ; Zeroes %eax
        (emit out-port "\tsete %al")                   ; Set lower byte of %eax to 1 if comparison was successful
        (emit out-port "\tsall $~a, %eax" bool-shift)  ; Apply appropriate shift
        (emit out-port "\torl $~a, %eax" bool-tag))    ; and tag
      (error "emit-expr" "zero? can only be called with integers")))

  (define-primitive (null? out-port si arg)
    (emit-expr out-port si arg)
    (emit out-port "\tcmpl $~a, %eax" empty-list-value) ; Compares %eax to empty-list-value
    (emit out-port "\tmovl $0, %eax")                   ; Zeroes %eax
    (emit out-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
    (emit out-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
    (emit out-port "\torl $~a, %eax" bool-tag))         ; and tag

  (define-primitive (not out-port si arg)
     (emit-expr out-port si arg)
     (emit out-port "\tcmpl $~a, %eax" bool-tag)         ; Compares %eax to the representation of false
     (emit out-port "\tmovl $0, %eax")                   ; Zeroes %eax
     (emit out-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
     (emit out-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
     (emit out-port "\torl $~a, %eax" bool-tag))         ; and tag

  (define-primitive (integer? out-port si arg)
    (emit-expr out-port si arg)
    (emit out-port "\tand $~a, %eax" fixnum-tag-mask)   ; Gets the tag of a fixnum
    (emit out-port "\tcmpl $~a, %eax" fixnum-tag)       ; Compares %eax to a tag of a fixnum
    (emit out-port "\tmovl $0, %eax")                   ; Zeroes %eax
    (emit out-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
    (emit out-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
    (emit out-port "\torl $~a, %eax" bool-tag))         ; and tag

  (define-primitive (boolean? out-port si arg)
    (emit-expr out-port si arg)
    (emit out-port "\tand $~a, %eax" bool-tag-mask)     ; Gets the tag of a boolean
    (emit out-port "\tcmpl $~a, %eax" bool-tag)         ; Compares %eax to bool-tag
    (emit out-port "\tmovl $0, %eax")                   ; Zeroes %eax
    (emit out-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
    (emit out-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
    (emit out-port "\torl $~a, %eax" bool-tag))         ; and tag

  (define-primitive (char? out-port si arg)
    (emit-expr out-port si arg)
    (emit out-port "\tand $~a, %eax" char-tag-mask)     ; Gets the tag of a char
    (emit out-port "\tcmpl $~a, %eax" char-tag)         ; Compares %eax to char-tag
    (emit out-port "\tmovl $0, %eax")                   ; Zeroes %eax
    (emit out-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
    (emit out-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
    (emit out-port "\torl $~a, %eax" bool-tag))         ; and tag

  (define-primitive (lognot out-port si arg)
    (if (integer? arg)
      (begin
        (emit-expr out-port si arg)
        (emit out-port "\txorl $0xFFFFFFFC, %eax")) ; Flip all bits except the last two
      (error "emit-expr" "lognot can only be called with integers")))

  ; TODO: Is shifting necessary?
  (define-primitive (+ out-port si arg1 arg2)
    (emit-expr out-port si arg1)
    (emit out-port "\tmovl %eax, ~s(%rsp)" si)
    (emit-expr out-port (- si word-size) arg2)
    (emit out-port "\taddl ~s(%rsp), %eax" si))

  ; TODO: Is shifting necessary?
  (define-primitive (- out-port si arg1 arg2)
    (emit-expr out-port si arg2)
    (emit out-port "\tmovl %eax, ~s(%rsp)" si)
    (emit-expr out-port (- si word-size) arg1)
    (emit out-port "\tsubl ~s(%rsp), %eax" si))

  (define-primitive (* out-port si arg1 arg2)
    (emit-expr out-port si arg1)
    (emit out-port "\tshr $~a, %eax" fixnum-shift)
    (emit out-port "\tmovl %eax, ~s(%rsp)" si)
    (emit-expr out-port (- si word-size) arg2)
    (emit out-port "\tshr $~a, %eax" fixnum-shift)
    (emit out-port "\timul ~s(%rsp), %eax" si)
    (emit out-port "\tshl $~a, %eax" fixnum-shift))

  (define-primitive (= out-port si arg1 arg2)
    (emit-binary-comparison '= out-port si arg1 arg2))

  (define-primitive (< out-port si arg1 arg2)
    (emit-binary-comparison '< out-port si arg1 arg2))

  (define-primitive (<= out-port si arg1 arg2)
    (emit-binary-comparison '<= out-port si arg1 arg2))

  (define-primitive (> out-port si arg1 arg2)
    (emit-binary-comparison '> out-port si arg1 arg2))

  (define-primitive (>= out-port si arg1 arg2)
    (emit-binary-comparison '>= out-port si arg1 arg2))

  (define-primitive (char=? out-port si arg1 arg2)
    (emit-binary-comparison '= out-port si arg1 arg2))

  (define-primitive (char<? out-port si arg1 arg2)
    (emit-binary-comparison '< out-port si arg1 arg2))

  (define-primitive (char<=? out-port si arg1 arg2)
    (emit-binary-comparison '<= out-port si arg1 arg2))

  (define-primitive (char>? out-port si arg1 arg2)
    (emit-binary-comparison '> out-port si arg1 arg2))

  (define-primitive (char>=? out-port si arg1 arg2)
    (emit-binary-comparison '>= out-port si arg1 arg2))
  ; ******* Definition of primitives ******
)
