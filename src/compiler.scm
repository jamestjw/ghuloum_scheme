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
  (define vector-tag (string->number "010" 2))
  (define string-tag (string->number "011" 2))
  (define closure-tag (string->number "110" 2))

  (define word-size 8)

  (define (out-file) (open-output-file "output.s" 'truncate))

  (define (next-index-by-n op si n) (apply op (list si (* n word-size))))

  (define (next-stack-index-by-n si n) (next-index-by-n - si n))

  (define (next-stack-index si) (next-index-by-n - si 1))

  (define (next-heap-index hi) (+ hi word-size))

  (define (next-heap-index-by-n si n) (next-index-by-n + si n))

  ; One port for code segment and another for text segment
  (define-record-type output-ports (fields fns main in-fn))

  ; TODO: DRY this up
  (define (output-ports-fn-mode op) (make-output-ports (output-ports-fns op) (output-ports-main op) #t))
  (define (output-ports-main-mode op) (make-output-ports (output-ports-fns op) (output-ports-main op) #f))

  (define (emit out-port . args)
    (apply fprintf out-port args)
    (newline out-port))

  (define (emit-function-header out-port fn-name)
    (emit out-port "\t.text")
    (emit out-port (string-append "\t.globl " fn-name))
    ; Not needed by Mach-O assembler
    ; (emit out-port "\t.type scheme_entry, @function")
    (emit out-port (string-append fn-name ":")))

  (define (emit-return out-port)  (emit out-port "\tret"))

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
     [(_ (prim-name curr-port out-port si env arg* ...) b b* ...)
      (begin
        (putprop 'prim-name '*is-prim* #t)
        (putprop 'prim-name '*arg-count* (length '(arg* ...)))
        (putprop 'prim-name '*emitter* (lambda (curr-port out-port si env arg* ...) b b* ...)))]))

  (define primitives '(let let* if funcall closure lambda))

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

  (define (emit-primcall output-ports si env expr)
    (let ([prim (car expr)] [args (cdr expr)] [curr-port (get-current-port output-ports)])
      (check-primcall-args prim args) ; Verify that args are valid for this primcall
      (apply (primitive-emitter prim) curr-port output-ports si env args)))

  (define (emit-immediate out-port x)
    (emit out-port "\tmovq $~a, %rax" (immediate-rep x)))

  (define (emit-call-label out-port label)
    (emit out-port "\tcall ~a" label))

  (define (emit-call-address out-port)
    (emit out-port "\tcall *%rax"))

  ; TODO: Improve this
  (define (variable? expr) (and (not (or (primitive? expr) (contains? primitives expr))) (symbol? expr)))

  ; TODO: This function should potentially also verify the structure
  ; of the body etc
  (define (let? expr) (and
                        (atom? (car expr))
                        (contains? '(let let* letrec) (car expr))))

  ; TODO: Verify structure of the body
  (define (if? expr) (eq? (car expr) 'if))

  (define (labels? expr) (eq? (car expr) 'labels))

  (define (funcall? expr) (eq? (car expr) 'funcall))

  (define (closure? expr) (eq? (car expr) 'closure))

  (define (lambda? expr) (eq? (car expr) 'lambda))

  (define (get-current-port out-port) (if (output-ports-in-fn out-port) (output-ports-fns out-port) (output-ports-main out-port)))

  (define (emit-stack-save out-port si)
    (emit-copy-register-stack out-port si "rax"))

  (define (emit-copy-register-stack out-port si register)
    (emit out-port "\tmovq %~a, ~s(%rsp)" register si))

  (define (extend-env sym-name location old-env) (cons (cons sym-name location) old-env))

  (define (lookup-env sym-name env)
    (let ([res (assq sym-name env)])
      (if res (cdr res) #f)))

  (define-record-type closure-ptr-offset (fields offset))

  (define-record-type letrec-stack-offset (fields offset))

  (define (emit-let out-port si env expr)
    (define bindings (cadr expr))
    (define body (caddr expr))
    (define let-flavor (car expr)) ; Either `let`, `let*` or `letrec`
    (define input-env
      (if (eq? let-flavor 'letrec)
      ; Preallocate the vars on the stack
      ; Reverse as this method does it in the reverse order but in the process-let
      ; function below we go from left to right.
      (car (extend-env-with-letrec-vars (map car bindings) env si)) ; TODO: Avoid ugly `car` hack here
      env))
    (define input-si
      (if (eq? let-flavor 'letrec)
          (next-stack-index-by-n si (length bindings)) ; We need the space to preallocate the vars in the let-binding
          si))
    (let process-let ([b* bindings] [si input-si] [new-env input-env])
      (cond
        [(null? b*)
          (if (eq? let-flavor 'letrec)
              (begin
                (for-each (lambda (v-loc)
                  (emit-expr out-port si input-env (car v-loc))
                  (emit (get-current-port out-port) "\tmovq ~a(%r14), %r15" (cdr v-loc))
                  (emit (get-current-port out-port) "\tmovq %rax, 0(%r15)"))
                  letrec-backfill-list)
                (clear-letrec-backfill))
              '())
          (emit-expr out-port si new-env body)]
        ; Here we assume that `b` is a pair
        ; For the classic `let`, we use the environment that was passed in,
        ; otherwise we use the updated environment as variables are binded
        [else (cond
                [(eq? let-flavor 'let) (let ([b (car b*)]
                                              [env-to-use env]) ; Use the original environment
                                              (emit-expr out-port si env-to-use (cadr b))
                                              (emit-stack-save (get-current-port out-port) si)
                                              (process-let
                                                (cdr b*)
                                                (next-stack-index si)
                                                (extend-env (car b) si new-env)))]
                [(eq? let-flavor 'let*) (let ([b (car b*)]
                                              [env-to-use new-env]) ; Use the new environment
                                              (emit-expr out-port si env-to-use (cadr b))
                                              (emit-stack-save (get-current-port out-port) si)
                                              (process-let
                                                (cdr b*)
                                                (next-stack-index si)
                                                (extend-env (car b) si new-env)))]
                [(eq? let-flavor 'letrec) (let ([b (car b*)]
                                                [env-to-use new-env]) ; Use the new environment
                                                (emit-expr out-port si env-to-use (cadr b))
                                                (emit-stack-save (get-current-port out-port) (letrec-stack-offset-offset (lookup-env (car b) new-env)))
                                                (process-let
                                                  (cdr b*)
                                                  si
                                                  new-env))]
                [else (error "emit-let" (string-append "Unknown let flavor encountered: " (symbol->string let-flavor)))])]))
)

  (define (emit-stack-load out-port si) (emit-stack-load-register out-port si "rax"))
  (define (emit-stack-load-register out-port si register) (emit out-port "\tmovq ~s(%rsp), %~a" si register))

  (define (emit-variable-ref out-port env var)
    (let ([res (lookup-env var env)])
      (cond
        [res (cond
          [(closure-ptr-offset? res) (emit out-port "\tmovq ~a(%r12), %rax" (closure-ptr-offset-offset res))]
          [(letrec-stack-offset? res) (emit out-port "\tmovq ~a(%rsp), %rax" (letrec-stack-offset-offset res))]
          [else (emit-stack-load out-port res)])]
        [else (error "emit-variable-ref" (string-append "Variable not found in scope: " (symbol->string var)))])))

  (define (emit-je out-port label) (emit out-port "\tje ~a" label))
  (define (emit-jne out-port label) (emit out-port "\tjne ~a" label))
  (define (emit-jmp out-port label) (emit out-port "\tjmp ~a" label))
  (define (emit-label out-port label) (emit out-port "~a:" label))

  (define (emit-if out-port si env expr)
    (define (if-condition e) (cadr e))
    (define (true-body e) (caddr e))
    (define (false-body e) (cadddr e))

    (let ([label-1 (unique-label)] [label-2 (unique-label)] [curr-port (get-current-port out-port)])
      (emit-expr out-port si env (if-condition expr))
      (emit curr-port "\tcmp $~s, %al" (immediate-rep #f)) ; Anything that is not #f is truthy
      (emit-je curr-port label-1)
      (emit-expr out-port si env (true-body expr))
      (emit-jmp curr-port label-2)
      (emit-label curr-port label-1)
      (emit-expr out-port si env (false-body expr))
      (emit-label curr-port label-2)))

  ; Returns new stack index and environment
  (define (extend-env-with-formals formals env si)
    (extend-env-with-vars-in-stack formals env si))

  (define (extend-env-with-vars-in-stack vars env si)
    (fold-left
      (lambda (acc var) (cons (extend-env var (cdr acc) (car acc)) (next-stack-index (cdr acc))))
      (cons env si)
      vars))

  (define (extend-env-with-letrec-vars vars env si)
    (fold-left
      (lambda (acc var)
              (cons
                (extend-env var (make-letrec-stack-offset (cdr acc)) (car acc))
                (next-stack-index (cdr acc))))
      (cons env si)
      vars))

  (define (extend-env-with-free-vars vars env)
    (car (fold-left
      (lambda (acc var) (cons (extend-env var (make-closure-ptr-offset (cdr acc)) (car acc)) (next-heap-index (cdr acc))))
      (cons env word-size) ; Start with an offset of 1 word size as the first slot is for the label
      vars)))

  ; Handle this syntax
  ; (code (var ...) <Expr>)
  (define (emit-code out-port env label expr)
    (let*
      ([curr-port (get-current-port out-port)]
        [formals (cadr expr)]
        [free-vars (caddr expr)]
        [body (cadddr expr)]
        [new-env-si (extend-env-with-formals
                      formals
                      (extend-env-with-free-vars free-vars env)
                      (- (* 2 word-size)))] ; Skip two slots for the ret and closure ptr
        [env (car new-env-si)]
        [si (cdr new-env-si)])
      (emit-function-header curr-port label)
      (emit-expr out-port si env body)
      (emit-return curr-port)))

  ; Handle this syntax
  ;   (labels ((lvar <LExpr>) ...) <Expr>)
  ; where <LExpr> ::= (code (var ...) <Expr>)
  (define (emit-labels out-port si env expr)
    ; TODO: Ensure that the expression has the right structure
    (let*
      ([lvars (cadr expr)]
        [labels (map car lvars)]
        [lexprs (map cadr lvars)]
        [body (caddr expr)]
        [new-out-port (output-ports-fn-mode out-port)]
        [curr-port (get-current-port out-port)]
        [new-env (fold-left (lambda (env label lexpr)
                    (let* ([l (unique-label)] [new-env (extend-env label l env)])
                      (emit-code new-out-port new-env l lexpr)
                      new-env))
                    env
                    labels
                    lexprs)])
      (emit-expr out-port si new-env body))) ; Emit body using original port

  ; Handles this syntax
  ; (funcall lvar <Expr> ...)
  (define (emit-funcall out-port si env expr)
    ; TODO: Add validation of lvar
    (let ([lvar (cadr expr)] [args (cddr expr)] [curr-port (get-current-port out-port)])
      (fold-left
        (lambda (si arg)
          (emit-expr out-port si env arg)
          (emit-stack-save curr-port si)
          (next-stack-index si))
        (next-stack-index (next-stack-index si)) ; Skip two stack indexes as we use it for the return address
        args)
      ; Assume that have a closure in %rax
      (emit-expr out-port si env lvar)
      (emit curr-port "\tmovq %r12, ~a(%rsp)" (next-stack-index si)) ; Store value of r12 on the stack before we assign closure pointer
      (emit curr-port "\txorq $~a, %rax" closure-tag) ; Remove closure-tag
      (emit curr-port "\tmovq %rax, %r12") ; Store closure pointer in r12
      (emit curr-port "\tmovq 0(%rax), %rax") ; Copy address of the closure's label to rax
      ; Shift stack pointer to right spot before function call
      (emit curr-port "\taddq $~a, %rsp" (+ si word-size))
      (emit-call-address curr-port)
      ; Restore stack pointer
      (emit curr-port "\tsubq $~a, %rsp" (+ si word-size))
      (emit curr-port "\tmovq ~a(%rsp), %r12" (next-stack-index si)))) ; Restore value of r12

  ; TODO: Verify that the body of the expression conforms to a closure call
  (define (emit-closure out-port si env expr)
    (let* ([lvar-label (lookup-env (cadr expr) env)]
            [vars (cddr expr)] [curr-port (get-current-port out-port)]
            [num-vars (length vars)])
      (emit curr-port "\tlea ~a(%rip), %r13" lvar-label) ; Store effective address of label
      (emit curr-port "\tmovq %r13, 0(%rbp)")
      (emit curr-port "\tmovq %rbp, %rdi") ; Store the heap pointer
      ; Advance the heap pointer
      ; First slot points to the label
      ; Remaining slots contain the free variables
      (emit curr-port "\taddq $~a,  %rbp" (+ word-size (* num-vars word-size)))
      (fold-left
        (lambda (hi var)
                (let ([loc (lookup-env var env)])
                  ; If the offset is greater than where we currently are, it means
                  ; that the function will not be defined yet and will have to be backfilled later
                  ; Since we generate closures, we can be sure that expr is a var
                  (if (letrec-stack-offset? loc)
                    (begin
                      (emit curr-port "\tleaq ~a(%rdi), %rax" hi)
                      (emit curr-port "\tmovq %rax, ~a(%r14)" (add-letrec-backfill var)))
                    (begin
                      (emit-expr out-port hi env var)
                      (emit curr-port "\tmovq %rax, ~a(%rdi)" hi)
                      (next-heap-index hi)))))

        word-size ; Start with +1 * wordsize as the first slot is for the label
        vars)
      (emit curr-port "\tmovq %rdi,  %rax")
      (emit curr-port "\torq $~a,  %rax" closure-tag))) ; Add closure tag

  ; Converts a lambda expression to one that is recognised by our code generator
  (define (convert-lambda expr)
    (cond
      [(atom? expr) expr] ; If it is an atom, just return it
      [(eq? 'lambda (car expr))
          (let* ([formals (cadr expr)]
            [body (caddr expr)]
            ; Free vars are variables that are not formals
            [free-vars (find-fvs body formals)])
            (list 'lambda formals free-vars (convert-lambda body)))] ; Handle the case when it is a lambda
      [else (map convert-lambda expr)]))

  (define (emit-expr out-port si env expr)
    (let ([curr-port (if (output-ports-in-fn out-port) (output-ports-fns out-port) (output-ports-main out-port))])
      (cond
        [(immediate? expr) (emit-immediate curr-port expr)]
        [(variable? expr) (emit-variable-ref curr-port env expr)]
        [(let? expr) (emit-let out-port si env expr)]
        [(if? expr) (emit-if out-port si env expr)]
        [(primcall? expr) (emit-primcall out-port si env expr)]
        [(labels? expr) (emit-labels out-port si env expr)]
        [(funcall? expr) (emit-funcall out-port si env expr)]
        [(closure? expr) (emit-closure out-port si env expr)]
        [(eq? '() (eval expr)) (emit-expr out-port si env '())] ; Workaround until we can deal with quotes
        [else (error "emit-expr" (string-append "Unknown expression " (sexpr->string expr) " encountered"))])))

  ; Generates a unique label, e.g. (unique-label) => L_0
  (define unique-label
    (let ([count 0])
      (lambda ()
        (let ([L (format "_L_~s" count)])
          (set! count (add1 count))
          L))))

  (define letrec-backfill-list '())

  (define letrec-backfill-location 0)

  (define (clear-letrec-backfill) (set! letrec-backfill-list '()) (set! letrec-backfill-location 0))

  (define (add-letrec-backfill sym-name)
    (let ([loc letrec-backfill-location])
        ; Check if we have used too much space for the backfill list
        (if (eq? loc (next-heap-index-by-n 0 20))
          (error "Letrec backfill" "Exceeded limit")
          (begin
            (set!
              letrec-backfill-list
              (cons (cons sym-name letrec-backfill-location) letrec-backfill-list))
            (set! letrec-backfill-location (next-heap-index letrec-backfill-location))
            loc))))

  (define (emit-entrypoint out-ports)
    (emit-function-header (output-ports-fns out-ports) "_scheme_entry")
    (emit (output-ports-fns out-ports) "\tmovq %rdi, %rcx") ; Get address of struct to store register values (from 1st arg of _scheme_entry)
    ; Store registers in context struct
    (emit (output-ports-fns out-ports) "\tmovq %rbx, 8(%rcx)")
    (emit (output-ports-fns out-ports) "\tmovq %rsi, 32(%rcx)")
    (emit (output-ports-fns out-ports) "\tmovq %rdi, 40(%rcx)")
    (emit (output-ports-fns out-ports) "\tmovq %rbp, 48(%rcx)")
    (emit (output-ports-fns out-ports) "\tmovq %rsp, 56(%rcx)")

    (emit (output-ports-fns out-ports) "\tmovq %rsi, %rsp") ; Store stack pointer in rsp
    (emit (output-ports-fns out-ports) "\tmovq %rdx, %rbp") ; Store heap pointer in rbp
    (emit (output-ports-fns out-ports) "\tmovq %rbp, %r14") ; Use r14 as the pointer for letrec backfills
    (emit (output-ports-fns out-ports) "\taddq $~a, %rbp" (next-heap-index-by-n 0 20)) ; Reserve 20 slots for letrec backfills

    (emit (output-ports-fns out-ports) "\tcall _L_scheme_entry")

    ; Restore values in context struct
    (emit (output-ports-fns out-ports) "\tmovq 8(%rcx), %rbx")
    (emit (output-ports-fns out-ports) "\tmovq 32(%rcx), %rsi")
    (emit (output-ports-fns out-ports) "\tmovq 40(%rcx), %rdi")
    (emit (output-ports-fns out-ports) "\tmovq 48(%rcx), %rbp")
    (emit (output-ports-fns out-ports) "\tmovq 56(%rcx), %rsp")
    (emit (output-ports-fns out-ports) "\tret"))

  (define (compile-program expr)
    ; Define outport out of function
    (define out-ports (make-output-ports (open-output-string) (open-output-string) #f))

    (emit-preamble (output-ports-fns out-ports))

    (emit-function-header (output-ports-main out-ports) "_L_scheme_entry")
    (emit-expr out-ports (- word-size) '() expr)
    (emit (output-ports-main out-ports) "\tret")

    (emit-entrypoint out-ports)

    (flush-output-port (output-ports-fns out-ports))
    (flush-output-port (output-ports-main out-ports))

    (let ([of (out-file)])
      (fprintf of (get-output-string (output-ports-fns out-ports)))
      (fprintf of "\n")
      (fprintf of (get-output-string (output-ports-main out-ports)))
      (close-port of)))

  (define (emit-binary-comparison op curr-port out-port si env arg1 arg2)
    (define asm_op (case op
      ((=) "sete")
      ((>) "setg")
      ((>=) "setge")
      ((<) "setl")
      ((<=) "setle")
      (else (error "emit-binary-comparison" "Invalid operator"))))
    (emit-expr out-port si env arg1)
    (emit-stack-save curr-port si)
    (emit-expr out-port (next-stack-index si) env arg2)
    (emit curr-port "\tcmpl %eax, ~s(%rsp)" si)
    (emit curr-port "\tmovl $0, %eax")
    (emit curr-port "\t~s %al" asm_op)
    (emit curr-port "\tsall $~a, %eax" bool-shift)
    (emit curr-port "\torl $~a, %eax" bool-tag))

  ; Handle nested lambdas
  (define (converted-lambda-to-closure expr)
    ; Whenever it encounters a lambda, it returns an equivalent closure and list of codes.
    ; If the expression is not a lambda, it returns the same expression with an empty list of codes.
    (define (extract-lambda expr)
        (cond
          [(atom? expr) (list expr '())]
          [(lambda? expr)
                (let* ([formals (cadr expr)]
                      [free-vars (caddr expr)]
                      [body (cadddr expr)]
                      [converted-body-code (extract-lambda body)]
                      [converted-body (car converted-body-code)]
                      [extracted-codes (cadr converted-body-code)]
                      [l (string->symbol (unique-label))])
                  (list
                    (append (list 'closure l) free-vars) ; Replace lambda with closure expression
                    (append extracted-codes (list (list
                                                    l
                                                    (list 'code formals free-vars converted-body))))))]
          [else (let ([res (map extract-lambda expr)])
                      (list
                        (map car res)
                        (apply append (map cadr res))))]))
    (let ([res (extract-lambda expr)])
      (list 'labels (cadr res) (car res))))

  ; Returns a list of free variables in the given expression
  (define (find-fvs expr env)
    (define (find-fvs-for-list exprs env)
      (fold-left (lambda (freevars binding)
                (append
                  freevars
                  (find-fvs binding env)))
                '()
                exprs))
    (cond
      [(immediate? expr) '()]
      [(variable? expr) (if (contains? env expr) '() (list expr))]
      [(let? expr) (let ([new-env (append (map car (cadr expr)) env)])
                     (append
                       (find-fvs-for-list (map cadr (cadr expr)) env)  ; Apply this to rhs of bindings
                       (find-fvs (caddr expr) new-env)))]
      [(if? expr) (find-fvs-for-list (cdr expr) env)]
      [(funcall? expr) (find-fvs-for-list (cdr expr) env)]
      [(lambda? expr) (find-fvs-for-list (cddr expr) (append (cadr expr) env))] ; Doing this before primcall
      [(primcall? expr) (find-fvs-for-list (cdr expr) env)]
      [(labels? expr) '()] ; We don't expect users to use this form, hence its fine to skip the implementation
      [(closure? expr) (find-fvs-for-list (cdr expr) env)]
      [else '()]))

  ; ******* Definition of primitives ******
  (define-primitive (add1 curr-port out-port si env arg)
    (emit-expr out-port si env arg)
    (emit curr-port "\taddq $~s, %rax" (immediate-rep 1)))

  (define-primitive (sub1 curr-port out-port si env arg)
    (emit-expr out-port si env arg)
    (emit curr-port "\tsubq $~s, %rax" (immediate-rep 1)))

  (define-primitive (integer->char curr-port out-port si env arg)
    (emit-expr out-port si env arg)
    (emit curr-port "\tshl $~a, %eax" fixnum2char-shift)
    (emit curr-port "\tor $~a, %eax" char-tag))

  (define-primitive (char->integer curr-port out-port si env arg)
    (emit-expr out-port si env arg)
    (emit curr-port "\tshr $~a, %eax" char2fixnum-shift))

  (define-primitive (zero? curr-port out-port si env arg)
    (emit-expr out-port si env arg)
    (emit curr-port "\tcmpl $0, %eax")              ; Compares %eax to 0
    (emit curr-port "\tmovl $0, %eax")              ; Zeroes %eax
    (emit curr-port "\tsete %al")                   ; Set lower byte of %eax to 1 if comparison was successful
    (emit curr-port "\tsall $~a, %eax" bool-shift)  ; Apply appropriate shift
    (emit curr-port "\torl $~a, %eax" bool-tag))    ; and tag)

  (define-primitive (null? curr-port out-port si env arg)
    (emit-expr out-port si env arg)
    (emit curr-port "\tcmpl $~a, %eax" empty-list-value) ; Compares %eax to empty-list-value
    (emit curr-port "\tmovl $0, %eax")                   ; Zeroes %eax
    (emit curr-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
    (emit curr-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
    (emit curr-port "\torl $~a, %eax" bool-tag))         ; and tag

  (define-primitive (not curr-port out-port si env arg)
     (emit-expr out-port si env arg)
     (emit curr-port "\tcmpl $~a, %eax" bool-tag)         ; Compares %eax to the representation of false
     (emit curr-port "\tmovl $0, %eax")                   ; Zeroes %eax
     (emit curr-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
     (emit curr-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
     (emit curr-port "\torl $~a, %eax" bool-tag))         ; and tag

  (define-primitive (integer? curr-port out-port si env arg)
    (emit-expr out-port si env arg)
    (emit curr-port "\tand $~a, %eax" fixnum-tag-mask)   ; Gets the tag of a fixnum
    (emit curr-port "\tcmpl $~a, %eax" fixnum-tag)       ; Compares %eax to a tag of a fixnum
    (emit curr-port "\tmovl $0, %eax")                   ; Zeroes %eax
    (emit curr-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
    (emit curr-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
    (emit curr-port "\torl $~a, %eax" bool-tag))         ; and tag

  (define-primitive (boolean? curr-port out-port si env arg)
    (emit-expr out-port si env arg)
    (emit curr-port "\tand $~a, %eax" bool-tag-mask)     ; Gets the tag of a boolean
    (emit curr-port "\tcmpl $~a, %eax" bool-tag)         ; Compares %eax to bool-tag
    (emit curr-port "\tmovl $0, %eax")                   ; Zeroes %eax
    (emit curr-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
    (emit curr-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
    (emit curr-port "\torl $~a, %eax" bool-tag))         ; and tag

  (define-primitive (char? curr-port out-port si env arg)
    (emit-expr out-port si env arg)
    (emit curr-port "\tand $~a, %eax" char-tag-mask)     ; Gets the tag of a char
    (emit curr-port "\tcmpl $~a, %eax" char-tag)         ; Compares %eax to char-tag
    (emit curr-port "\tmovl $0, %eax")                   ; Zeroes %eax
    (emit curr-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
    (emit curr-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
    (emit curr-port "\torl $~a, %eax" bool-tag))         ; and tag

  (define-primitive (lognot curr-port out-port si env arg)
    (if (integer? arg)
      (begin
        (emit-expr out-port si env arg)
        (emit curr-port "\txorq $0xFFFFFFFFFFFFFFFC, %rax")) ; Flip all bits except the last two
      (error "emit-expr" "lognot can only be called with integers")))

  ; TODO: Is shifting necessary?
  (define-primitive (+ curr-port out-port si env arg1 arg2)
    (emit-expr out-port si env arg1)
    (emit-stack-save curr-port si)
    (emit-expr out-port (next-stack-index si) env arg2)
    (emit curr-port "\taddq ~s(%rsp), %rax" si))

  ; TODO: Is shifting necessary?
  (define-primitive (- curr-port out-port si env arg1 arg2)
    (emit-expr out-port si env arg2)
    (emit-stack-save curr-port si)
    (emit-expr out-port (next-stack-index si) env arg1)
    (emit curr-port "\tsubq ~s(%rsp), %rax" si))

  (define-primitive (* curr-port out-port si env arg1 arg2)
    (emit-expr out-port si env arg1)
    (emit-stack-save curr-port si)
    (emit-expr out-port (next-stack-index si) env arg2)
    (emit curr-port "\tshr $~a, %eax" fixnum-shift)
    (emit curr-port "\timulq ~s(%rsp), %rax" si))

  (define-primitive (= curr-port out-port si env arg1 arg2)
    (emit-binary-comparison '= curr-port out-port si env arg1 arg2))

  (define-primitive (< curr-port out-port si env arg1 arg2)
    (emit-binary-comparison '< curr-port out-port si env arg1 arg2))

  (define-primitive (<= curr-port out-port si env arg1 arg2)
    (emit-binary-comparison '<= curr-port out-port si env arg1 arg2))

  (define-primitive (> curr-port out-port si env arg1 arg2)
    (emit-binary-comparison '> curr-port out-port si env arg1 arg2))

  (define-primitive (>= curr-port out-port si env arg1 arg2)
    (emit-binary-comparison '>= curr-port out-port si env arg1 arg2))

  (define-primitive (char=? curr-port out-port si env arg1 arg2)
    (emit-binary-comparison '= curr-port out-port si env arg1 arg2))

  (define-primitive (char<? curr-port out-port si env arg1 arg2)
    (emit-binary-comparison '< curr-port out-port si env arg1 arg2))

  (define-primitive (char<=? curr-port out-port si env arg1 arg2)
    (emit-binary-comparison '<= curr-port out-port si env arg1 arg2))

  (define-primitive (char>? curr-port out-port si env arg1 arg2)
    (emit-binary-comparison '> curr-port out-port si env arg1 arg2))

  (define-primitive (char>=? curr-port out-port si env arg1 arg2)
    (emit-binary-comparison '>= curr-port out-port si env arg1 arg2))

  (define-primitive (cons curr-port out-port si env arg1 arg2)
    (emit-copy-register-stack curr-port si "rbx") ; Store current value of rbx so we can use it to store heap pointer
    (emit curr-port "\tmovq %rbp, %rbx")
    (emit curr-port "\taddq $16, %rbp")
    (emit-expr out-port (next-stack-index si) env arg2)
    (emit curr-port "\tmovq %rax, 8(%rbx)")
    (emit-expr out-port (next-stack-index si) env arg1)
    (emit curr-port "\tmovq %rax, 0(%rbx)")
    (emit curr-port "\tmovq %rbx, %rax")
    (emit curr-port "\torq $~a, %rax" pair-tag)
    (emit-stack-load-register curr-port si "rbx")) ; Reload value of rbx

  (define-primitive (car curr-port out-port si env arg1)
    (emit-expr out-port si env arg1)
    (emit curr-port "\tmovq -1(%rax), %rax"))

  (define-primitive (cdr curr-port out-port si env arg1)
    (emit-expr out-port si env arg1)
    (emit curr-port "\tmovq 7(%rax), %rax"))

  (define-primitive (make-vector curr-port out-port si env vec-length vec-elem)
    (emit-expr out-port si env vec-length)
    (emit curr-port "\tshr $~a, %eax" fixnum-shift) ; Remove fixnum shift
    (emit-stack-save curr-port si) ; Save num elems on the stack
    (emit curr-port "\tmovq %rax, 0(%rbp)") ; Store vec length

    ; Align to next object boundary
    (emit curr-port "\timulq $~a, %rax" word-size) ; Multiply vec length by word-size
    (emit curr-port "\tmovq %rax, %rdi") ; Copy the length to rdi to calculate word aligned vec length
    (emit curr-port "\taddq $15, %rdi")
    (emit curr-port "\tandq $-8, %rdi")

    (emit curr-port "\tmovq %rbp, %rsi") ; Save curr allocation pointer
    (emit curr-port "\taddq %rdi, %rbp "); Advance allocation pointer

    ; Populate vector with initial element
    ; TODO: This needs to be optimised
    (emit-copy-register-stack curr-port (next-stack-index si) "rdi") ; Save rdi and rsi on the stack before emitting new expression
    (emit-copy-register-stack curr-port (next-stack-index (next-stack-index si)) "rsi")
    (emit-expr out-port (next-stack-index (next-stack-index (next-stack-index si))) env vec-elem)
    (emit-stack-load-register curr-port (next-stack-index si) "rdi") ; Load values back to registers
    (emit-stack-load-register curr-port (next-stack-index (next-stack-index si)) "rsi")

    (let ([label-1 (unique-label)] [label-2 (unique-label)])
      (emit curr-port "\tmovq $0, ~s(%rsp)" (next-stack-index si)) ; Set up loop counter in stack
      (emit-jmp curr-port label-1)
      (emit-label curr-port label-2)
      (emit curr-port "\tmovq %rax, ~a(%rsi,%rdi,~a)" word-size word-size) ; Move rax to Vptr + counter*size + 0
      (emit curr-port "\taddq $1, ~s(%rsp)" (next-stack-index si)) ; Increment loop counter
      (emit-label curr-port label-1)
      (emit-stack-load-register curr-port (next-stack-index si) "rdi") ; Load loop counter to rdi
      (emit curr-port "\tcmpq ~s(%rsp), %rdi" si) ; Compare loop counter to num elems
      (emit curr-port "\tjl ~s" label-2)) ; Jump to next iteration if less than num elems

    ; Prepare final pointer to return
    (emit curr-port "\tmovq %rsi, %rax")
    (emit curr-port "\torq $~a, %rax" vector-tag)) ; Or with the vector tag

  (define-primitive (vector? curr-port out-port si env arg)
    (emit-expr out-port si env arg)
    (emit curr-port "\tandq $~a, %rax" vector-tag)       ; Gets the tag of a vector
    (emit curr-port "\tcmpq $~a, %rax" vector-tag)       ; Compares %eax to a tag of a vector
    (emit curr-port "\tmovl $0, %eax")                   ; Zeroes %eax
    (emit curr-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
    (emit curr-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
    (emit curr-port "\torl $~a, %eax" bool-tag))

  (define-primitive (vector-length curr-port out-port si env arg)
    (emit-expr out-port si env arg)
    (emit curr-port "\txorq $~a, %rax" vector-tag)    ; Remove the vector tag
    (emit curr-port "\tmovq 0(%rax), %rax")          ; Copy vector length
    (emit curr-port "\tshl $~a, %eax" fixnum-shift)) ; Apply fixnum shift

  (define-primitive (vector-ref curr-port out-port si env vec pos)
    (emit-expr out-port si env pos)
    (emit curr-port "\tshr $~a, %rax" fixnum-shift) ; Remove fixnum shift
    (emit-stack-save curr-port si) ; Save index on the stack

    (emit-expr out-port (next-stack-index si) env vec)
    (emit curr-port "\txorq $~a, %rax" vector-tag) ; Remove vector tag
    (emit-stack-load-register curr-port si "rdi") ; Load index to rdi
    ; Move from wordsz + vec-base + rdi * wordsz to eax
    ; Skip the first elem as we store the vector length there
    (emit curr-port "\tmovq ~a(%rax, %rdi, ~a), %rax" word-size word-size))

  (define-primitive (vector-set! curr-port out-port si env vec pos val)
    (emit-expr out-port si env pos)
    (emit curr-port "\tshr $~a, %rax" fixnum-shift) ; Remove fixnum shift
    (emit-stack-save curr-port si) ; Save index on the stack

    (emit-expr out-port (next-stack-index si) env val)
    (emit-stack-save curr-port (next-stack-index si)) ; Save value on the stack

    (emit-expr out-port (next-stack-index (next-stack-index si)) env vec)
    (emit curr-port "\txorq $~a, %rax" vector-tag) ; Remove vector tag
    (emit-stack-load-register curr-port si "rdi") ; Load index to rdi
    (emit-stack-load-register curr-port (next-stack-index si) "rsi") ; Load value to rsi
    (emit curr-port "\tmovq %rsi, ~a(%rax, %rdi, ~a)" word-size word-size)) ; Copy value to vector

  (define-primitive (make-string curr-port out-port si env str-length c)
    (emit-expr out-port si env str-length)
    (emit curr-port "\tshr $~a, %rax" fixnum-shift) ; Remove fixnum shift
    (emit-stack-save curr-port si) ; Save length on the stack
    (emit curr-port "\tmovq %rax, 0(%rbp)") ; Store str length

    ; Align to next object boundary
    (emit curr-port "\tmovq %rax, %rdi") ; Copy the length to rdi to calculate word aligned str length
    (emit curr-port "\taddq $15, %rdi")
    (emit curr-port "\tandq $-8, %rdi")

    (emit curr-port "\tmovq %rbp, %rsi") ; Save curr allocation pointer
    (emit curr-port "\taddq %rdi, %rbp "); Advance allocation pointer

    ; Populate string with initial element
    ; TODO: This needs to be optimised
    (emit-copy-register-stack curr-port (next-stack-index si) "rdi") ; Save rdi and rsi on the stack before emitting new expression
    (emit-copy-register-stack curr-port (next-stack-index (next-stack-index si)) "rsi")
    (emit-expr out-port (next-stack-index (next-stack-index (next-stack-index si))) env c)
    (emit curr-port "\tshr $~a, %rax" char-shift) ; Remove char shift
    (emit-stack-load-register curr-port (next-stack-index si) "rdi") ; Load values back to registers
    (emit-stack-load-register curr-port (next-stack-index (next-stack-index si)) "rsi")

    (let ([label-1 (unique-label)] [label-2 (unique-label)])
      (emit curr-port "\tmovq $0, ~s(%rsp)" (next-stack-index si)) ; Set up loop counter in stack
      (emit-jmp curr-port label-1)
      (emit-label curr-port label-2)
      (emit curr-port "\tmovb %al, ~a(%rsi,%rdi)" word-size) ; Move rax to Vptr + counter + wordsize
      (emit curr-port "\taddq $1, ~s(%rsp)" (next-stack-index si)) ; Increment loop counter
      (emit-label curr-port label-1)
      (emit-stack-load-register curr-port (next-stack-index si) "rdi") ; Load loop counter to rdi
      (emit curr-port "\tcmpq ~s(%rsp), %rdi" si) ; Compare loop counter to num elems
      (emit curr-port "\tjl ~s" label-2)) ; Jump to next iteration if less than num elems

    ; Prepare final pointer to return
    (emit curr-port "\tmovq %rsi, %rax")
    (emit curr-port "\torq $~a, %rax" string-tag))

  (define-primitive (string? curr-port out-port si env arg)
    (emit-expr out-port si env arg)
    (emit curr-port "\tandq $~a, %rax" string-tag)       ; Gets the tag of a string
    (emit curr-port "\tcmpq $~a, %rax" string-tag)       ; Compares %eax to a tag of a string
    (emit curr-port "\tmovl $0, %eax")                   ; Zeroes %eax
    (emit curr-port "\tsete %al")                        ; Set lower byte of %eax to 1 if comparison was successful
    (emit curr-port "\tsall $~a, %eax" bool-shift)       ; Apply appropriate shift
    (emit curr-port "\torl $~a, %eax" bool-tag))

  (define-primitive (string-length curr-port out-port si env arg)
    (emit-expr out-port si env arg)
    (emit curr-port "\txorq $~a, %rax" string-tag)   ; Remove the string tag
    (emit curr-port "\tmovq 0(%rax), %rax")          ; Copy string length
    (emit curr-port "\tshl $~a, %rax" fixnum-shift)) ; Apply fixnum shift

  (define-primitive (string-ref curr-port out-port si env s pos)
    (emit-expr out-port si env pos)
    (emit curr-port "\tshr $~a, %rax" fixnum-shift) ; Remove fixnum shift
    (emit-stack-save curr-port si) ; Save index on the stack

    (emit-expr out-port (next-stack-index si) env s)
    (emit curr-port "\txorq $~a, %rax" string-tag) ; Remove string tag
    (emit-stack-load-register curr-port si "rdi") ; Load index to rdi
    ; Move from wordsz + str-base + rdi to eax
    ; Skip the first elem as we store the str length there
    (emit curr-port "\tmovq ~a(%rax, %rdi), %rax" word-size)
    (emit curr-port "\tshl $~a, %rax" char-shift) ; Apply char shift and tag
    (emit curr-port "\torq $~a, %rax" char-tag))

  (define-primitive (string-set! curr-port out-port si env s pos c)
    (emit-expr out-port si env pos)
    (emit curr-port "\tshr $~a, %rax" fixnum-shift) ; Remove fixnum shift
    (emit-stack-save curr-port si) ; Save index on the stack

    (emit-expr out-port (next-stack-index si) env c)
    (emit curr-port "\tshr $~a, %rax" char-shift) ; Remove char tag
    (emit-stack-save curr-port (next-stack-index si)) ; Save value on the stack

    (emit-expr out-port (next-stack-index (next-stack-index si)) env s)
    (emit curr-port "\txorq $~a, %rax" string-tag) ; Remove string tag
    (emit-stack-load-register curr-port si "rdi") ; Load index to rdi
    (emit-stack-load-register curr-port (next-stack-index si) "rsi") ; Load value to rsi
    (emit curr-port "\tmovb %sil, ~a(%rax, %rdi)" word-size))

  (define-primitive (lambda curr-port out-port si env formals body)
    (let* ([converted-lambda-expr (convert-lambda (list 'lambda formals body))]
            [converted-closure-expr (converted-lambda-to-closure converted-lambda-expr)])
      (emit-expr out-port si env converted-closure-expr)))

  (define-primitive (or curr-port out-port si env expr1 expr2)
    (let ([end-label (unique-label)] [true-label (unique-label)])
      (emit-expr out-port si env expr1)
      (emit curr-port "\tcmp $~s, %al" (immediate-rep #f)) ; Anything that is not #f is truthy
      (emit-jne curr-port true-label)
      (emit-expr out-port si env expr2)
      (emit curr-port "\tcmp $~s, %al" (immediate-rep #f)) ; Anything that is not #f is truthy
      (emit-jne curr-port true-label)
      (emit-expr out-port si env #f) ; If we missed the jump, then emit a false
      (emit-jmp curr-port end-label)           ; And immediately jump to the end
      (emit-label curr-port true-label)
      (emit-expr out-port si env #t)
      (emit-label curr-port end-label)))

  (define-primitive (and curr-port out-port si env expr1 expr2)
    (let ([end-label (unique-label)] [false-label (unique-label)])
      (emit-expr out-port si env expr1)
      (emit curr-port "\tcmp $~s, %al" (immediate-rep #f)) ; Anything that is not #f is truthy
      (emit-je curr-port false-label)
      (emit-expr out-port si env expr2)
      (emit curr-port "\tcmp $~s, %al" (immediate-rep #f)) ; Anything that is not #f is truthy
      (emit-je curr-port false-label)
      (emit-expr out-port si env #t)    ; If we missed the jump, then emit a true
      (emit-jmp curr-port end-label)    ; And immediately jump to the end
      (emit-label curr-port false-label)
      (emit-expr out-port si env #f)
      (emit-label curr-port end-label)))
  ; ******* Definition of primitives ******
)
