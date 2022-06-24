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
  (define pair-tag (string->number "001" 2))

  (define word-size 8)

  (define (out-file) (open-output-file "output.s" 'truncate))

  (define (next-stack-index si) (- si word-size))

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

  ; Make it more convenient to define a primitive
  ; Binds the argument count and the code to emit
  ; the function call to a symbol corresponding
  ; to the name of the primitive
  (define-syntax define-primitive
    (syntax-rules ()
     [(_ (prim-name out-port si env arg* ...) b b* ...)
      (begin
        (putprop 'prim-name '*is-prim* #t)
        (putprop 'prim-name '*arg-count* (length '(arg* ...)))
        (putprop 'prim-name '*emitter* (lambda (out-port si env arg* ...) b b* ...)))]))

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

  (define (emit-primcall output-port si env expr)
    (let ([prim (car expr)] [args (cdr expr)])
      (check-primcall-args prim args) ; Verify that args are valid for this primcall
      (apply (primitive-emitter prim) output-port si env args)))

  (define (emit-immediate out-port x)
    (emit out-port "\tmovq $~a, %rax" (immediate-rep x)))

  ; TODO: Improve this
  (define (variable? expr) (symbol? expr))

  ; TODO: This function should potentially also verify the structure
  ; of the body etc
  (define (let? expr) (or (eq? (car expr) 'let) (eq? (car expr) 'let*)))

  ; TODO: Verify structure of the body
  (define (if? expr) (eq? (car expr) 'if))

  (define (emit-stack-save out-port si)
    (emit-copy-register-stack out-port si "rax"))

  (define (emit-copy-register-stack out-port si register)
    (emit out-port "\tmovq %~a, ~s(%rsp)" register si))

  (define (extend-env sym-name stack-index old-env) (cons (cons sym-name stack-index) old-env))

  (define (lookup-env sym-name env) (assq sym-name env))

  (define (emit-let out-port si env expr)
    (define bindings (cadr expr))
    (define body (caddr expr))
    (let process-let ([b* bindings] [si si] [new-env env])
      (cond
        [(null? b*) (emit-expr out-port si new-env body)]
        ; Here we assume that `b` is a pair
        ; For the classic `let`, we use the environment that was passed in,
        ; otherwise we use the updated environment as variables are binded
        [else (let ([b (car b*)] [env-to-use (if (eq? 'let (car expr)) env new-env)])
          (emit-expr out-port si env-to-use (cadr b))
          (emit-stack-save out-port si)
          (process-let (cdr b*) (next-stack-index si) (extend-env (car b) si new-env)))])))

  (define (emit-stack-load out-port si) (emit-stack-load-register out-port si "rax"))
  (define (emit-stack-load-register out-port si register) (emit out-port "\tmovq ~s(%rsp), %~a" si register))

  (define (emit-variable-ref out-port env var)
    (let ([res (lookup-env var env)])
      (cond
        [res (emit-stack-load out-port (cdr res))]
        [else (error "emit-variable-ref" (string-append "Variable not found in scope: " (symbol->string var)))])))

  (define (emit-je out-port label) (emit out-port "\tje ~a" label))
  (define (emit-jmp out-port label) (emit out-port "\tjmp ~a" label))
  (define (emit-label out-port label) (emit out-port "~a:" label))

  (define (emit-if out-port si env expr)
    (define (if-condition e) (cadr e))
    (define (true-body e) (caddr e))
    (define (false-body e) (cadddr e))

    (let ([label-1 (unique-label)] [label-2 (unique-label)])
      (emit-expr out-port si env (if-condition expr))
      (emit out-port "\tcmp $~s, %al" (immediate-rep #f)) ; Anything that is not #f is truthy
      (emit-je out-port label-1)
      (emit-expr out-port si env (true-body expr))
      (emit-jmp out-port label-2)
      (emit-label out-port label-1)
      (emit-expr out-port si env (false-body expr))
      (emit-label out-port label-2)))

  (define (emit-expr out-port si env expr)
    (cond
      [(immediate? expr) (emit-immediate out-port expr)]
      [(variable? expr) (emit-variable-ref out-port env expr)]
      [(let? expr) (emit-let out-port si env expr)]
      [(if? expr) (emit-if out-port si env expr)]
      [(primcall? expr) (emit-primcall out-port si env expr)]
      [(eq? '() (eval expr)) (emit-expr out-port si env '())] ; Workaround until we can deal with quotes
      [else (error "emit-expr" (string-append "Unknown expression " (sexpr->string expr) " encountered"))]))

  ; Generates a unique label, e.g. (unique-label) => L_0
  (define unique-label
    (let ([count 0])
      (lambda ()
        (let ([L (format "L_~s" count)])
          (set! count (add1 count))
          L))))

  (define (compile-program expr)
    (define of (out-file))

    (emit-preamble of)

    (emit-function-header of "_L_scheme_entry")
    (emit-expr of (- word-size) '() expr)
    (emit of "\tret")

    (emit-function-header of "_scheme_entry")
    (emit of "\tmovq %rdi, %rcx") ; Get address of struct to store register values (from 1st arg of _scheme_entry)
    ; Store registers in context struct
    (emit of "\tmovq %rbx, 8(%rcx)")
    (emit of "\tmovq %rsi, 32(%rcx)")
    (emit of "\tmovq %rdi, 40(%rcx)")
    (emit of "\tmovq %rbp, 48(%rcx)")
    (emit of "\tmovq %rsp, 56(%rcx)")

    (emit of "\tmovq %rsi, %rsp") ; Store stack pointer in rsp
    (emit of "\tmovq %rdx, %rbp") ; Store heap pointer in rbp

    (emit of "\tcall _L_scheme_entry")

    ; Restore values in context struct
    (emit of "\tmovq 8(%rcx), %rbx")
    (emit of "\tmovq 32(%rcx), %rsi")
    (emit of "\tmovq 40(%rcx), %rdi")
    (emit of "\tmovq 48(%rcx), %rbp")
    (emit of "\tmovq 56(%rcx), %rsp")
    (emit of "\tret")

    (flush-output-port of)
    (close-port of))

  (define (emit-binary-comparison op out-port si env arg1 arg2)
    (define asm_op (case op
      ((=) "sete")
      ((>) "setg")
      ((>=) "setge")
      ((<) "setl")
      ((<=) "setle")
      (else (error "emit-binary-comparison" "Invalid operator"))))
    (emit-expr out-port si env arg1)
    (emit-stack-save out-port si)
    (emit-expr out-port (next-stack-index si) env arg2)
    (emit out-port "\tcmpl %eax, ~s(%rsp)" si)
    (emit out-port "\tmovl $0, %eax")
    (emit out-port "\t~s %al" asm_op)
    (emit out-port "\tsall $~a, %eax" bool-shift)
    (emit out-port "\torl $~a, %eax" bool-tag))

  ; ******* Definition of primitives ******
  (define-primitive (add1 out-port si env arg)
    (emit-expr out-port si env arg)
    (emit out-port "\taddl $~s, %eax" (immediate-rep 1)))

  (define-primitive (sub1 out-port si env arg)
    (emit-expr out-port si env arg)
    (emit out-port "\tsubl $~s, %eax" (immediate-rep 1)))

  (define-primitive (integer->char out-port si env arg)
    (emit-expr out-port si env arg)
    (emit out-port "\tshl $~a, %eax" fixnum2char-shift)
    (emit out-port "\tor $~a, %eax" char-tag))

  (define-primitive (char->integer out-port si env arg)
    (emit-expr out-port si env arg)
    (emit out-port "\tshr $~a, %eax" char2fixnum-shift))

  (define-primitive (zero? out-port si env arg)
    (if (integer? arg)
      (begin
        (emit-expr out-port si env arg)
        (emit out-port "\tcmpl $0, %eax")              ; Compares %eax to 0
        (emit out-port "\tmovl $0, %eax")              ; Zeroes %eax
        (emit out-port "\tsete %al")                   ; Set lower byte of %eax to 1 if comparison was successful
        (emit out-port "\tsall $~a, %eax" bool-shift)  ; Apply appropriate shift
        (emit out-port "\torl $~a, %eax" bool-tag))    ; and tag
      (error "emit-expr" "zero? can only be called with integers")))

  (define-primitive (null? out-port si env arg)
    (emit-expr out-port si env arg)
    (emit out-port "\tcmpl $~a, %eax" empty-list-value) ; Compares %eax to empty-list-value
    (emit out-port "\tmovl $0, %eax")                   ; Zeroes %eax
    (emit out-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
    (emit out-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
    (emit out-port "\torl $~a, %eax" bool-tag))         ; and tag

  (define-primitive (not out-port si env arg)
     (emit-expr out-port si env arg)
     (emit out-port "\tcmpl $~a, %eax" bool-tag)         ; Compares %eax to the representation of false
     (emit out-port "\tmovl $0, %eax")                   ; Zeroes %eax
     (emit out-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
     (emit out-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
     (emit out-port "\torl $~a, %eax" bool-tag))         ; and tag

  (define-primitive (integer? out-port si env arg)
    (emit-expr out-port si env arg)
    (emit out-port "\tand $~a, %eax" fixnum-tag-mask)   ; Gets the tag of a fixnum
    (emit out-port "\tcmpl $~a, %eax" fixnum-tag)       ; Compares %eax to a tag of a fixnum
    (emit out-port "\tmovl $0, %eax")                   ; Zeroes %eax
    (emit out-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
    (emit out-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
    (emit out-port "\torl $~a, %eax" bool-tag))         ; and tag

  (define-primitive (boolean? out-port si env arg)
    (emit-expr out-port si env arg)
    (emit out-port "\tand $~a, %eax" bool-tag-mask)     ; Gets the tag of a boolean
    (emit out-port "\tcmpl $~a, %eax" bool-tag)         ; Compares %eax to bool-tag
    (emit out-port "\tmovl $0, %eax")                   ; Zeroes %eax
    (emit out-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
    (emit out-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
    (emit out-port "\torl $~a, %eax" bool-tag))         ; and tag

  (define-primitive (char? out-port si env arg)
    (emit-expr out-port si env arg)
    (emit out-port "\tand $~a, %eax" char-tag-mask)     ; Gets the tag of a char
    (emit out-port "\tcmpl $~a, %eax" char-tag)         ; Compares %eax to char-tag
    (emit out-port "\tmovl $0, %eax")                   ; Zeroes %eax
    (emit out-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
    (emit out-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
    (emit out-port "\torl $~a, %eax" bool-tag))         ; and tag

  (define-primitive (lognot out-port si env arg)
    (if (integer? arg)
      (begin
        (emit-expr out-port si env arg)
        (emit out-port "\txorq $0xFFFFFFFFFFFFFFFC, %rax")) ; Flip all bits except the last two
      (error "emit-expr" "lognot can only be called with integers")))

  ; TODO: Is shifting necessary?
  (define-primitive (+ out-port si env arg1 arg2)
    (emit-expr out-port si env arg1)
    (emit-stack-save out-port si)
    (emit-expr out-port (next-stack-index si) env arg2)
    (emit out-port "\taddl ~s(%rsp), %eax" si))

  ; TODO: Is shifting necessary?
  (define-primitive (- out-port si env arg1 arg2)
    (emit-expr out-port si env arg2)
    (emit-stack-save out-port si)
    (emit-expr out-port (next-stack-index si) env arg1)
    (emit out-port "\tsubl ~s(%rsp), %eax" si))

  (define-primitive (* out-port si env arg1 arg2)
    (emit-expr out-port si env arg1)
    (emit-stack-save out-port si)
    (emit-expr out-port (next-stack-index si) env arg2)
    (emit out-port "\tshr $~a, %eax" fixnum-shift)
    (emit out-port "\timul ~s(%rsp), %eax" si))

  (define-primitive (= out-port si env arg1 arg2)
    (emit-binary-comparison '= out-port si env arg1 arg2))

  (define-primitive (< out-port si env arg1 arg2)
    (emit-binary-comparison '< out-port si env arg1 arg2))

  (define-primitive (<= out-port si env arg1 arg2)
    (emit-binary-comparison '<= out-port si env arg1 arg2))

  (define-primitive (> out-port si env arg1 arg2)
    (emit-binary-comparison '> out-port si env arg1 arg2))

  (define-primitive (>= out-port si env arg1 arg2)
    (emit-binary-comparison '>= out-port si env arg1 arg2))

  (define-primitive (char=? out-port si env arg1 arg2)
    (emit-binary-comparison '= out-port si env arg1 arg2))

  (define-primitive (char<? out-port si env arg1 arg2)
    (emit-binary-comparison '< out-port si env arg1 arg2))

  (define-primitive (char<=? out-port si env arg1 arg2)
    (emit-binary-comparison '<= out-port si env arg1 arg2))

  (define-primitive (char>? out-port si env arg1 arg2)
    (emit-binary-comparison '> out-port si env arg1 arg2))

  (define-primitive (char>=? out-port si env arg1 arg2)
    (emit-binary-comparison '>= out-port si env arg1 arg2))

  (define-primitive (cons out-port si env arg1 arg2)
    (emit-copy-register-stack out-port si "rbx") ; Store current value of rbx so we can use it to store heap pointer
    (emit out-port "\tmovq %rbp, %rbx")
    (emit out-port "\taddq $16, %rbp")
    (emit-expr out-port (next-stack-index si) env arg2)
    (emit out-port "\tmovq %rax, 8(%rbx)")
    (emit-expr out-port (next-stack-index si) env arg1)
    (emit out-port "\tmovq %rax, 0(%rbx)")
    (emit out-port "\tmovq %rbx, %rax")
    (emit out-port "\torq $~a, %rax" pair-tag)
    (emit-stack-load-register out-port si "rbx") ; Reload value of rbx
    )

  (define-primitive (car out-port si env arg1)
    (emit-expr out-port si env arg1)
    (emit out-port "\tmovq -1(%rax), %rax"))

  (define-primitive (cdr out-port si env arg1)
    (emit-expr out-port si env arg1)
    (emit out-port "\tmovq 7(%rax), %rax"))
  ; ******* Definition of primitives ******
)
