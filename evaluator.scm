; The meta-circular evaluator for a Scheme-type language 
; without assignment.
; @author Johnny Eduardo Weber

(load "lst-environment.scm")

; ==== E v a l ====
(define (eval exp env)
  (cond
   ((self-evaluating? exp) exp)  
   ((variable? exp) (eval-variable exp env))
   ((quoted? exp) (eval-quotation exp env))
   ((definition? exp) (eval-definition exp env))
   ((if? exp) (eval-if exp env))
   ((lambda? exp) (eval-lambda exp env))
   ((begin? exp) (eval-sequence (begin-actions exp) env))
   ((cond? exp) (eval (cond->if exp) env))
   ((primitive? exp) (eval-primitive exp env))
   ((application? exp) (eval-application exp env))
   (else
    (error "Unknown expression type -- EVAL" exp))))

; ---- s e l f - e v a l u a t i n g ----
; exp is self-evaluting if it is a string or a number
(define (self-evaluating? exp) (or (string? exp) (number? exp)))

; ---- v a r i a b l e ----
; exp is a variable if it is a symbol
(define (variable? exp) (symbol? exp))
; to evaluate a variable, lookup variable value in env
(define (eval-variable exp env) (lookup-variable-value exp env))

; ---- q u o t e ----
; exp is a quotation if it is a list and it begins with 'quote
(define (quoted? exp) (if (pair? exp) (eq? (car exp) 'quote) #f))
; to evaluate a quotation, simply return the text after quote
(define (eval-quotation exp env) (cadr exp))

; ---- d e f i n e ----
; exp is a define if it is a list and it begins with 'define
(define (definition? exp) (if (pair? exp) (eq? (car exp) 'define) #f))
(define (definition-var exp) (cadr exp))
(define (definition-val exp) (caddr exp))
; to evaluate a define:
; (1) If the first param to define is a symbol, add mapping to env
; (2) If the first param to define is a pair, then it is a function
;     definition, so rewrite it as symbol -> lambda define.
(define (eval-definition exp env)
  (if (symbol? (definition-var exp))
      (if (empty-env? env) 
    (extend-environment 
	   (list (definition-var exp)) (list (eval (definition-val exp) env)) env)
	  (define-variable (definition-var exp) (eval (definition-val exp) env) env))
      (eval-definition (list 'define (caadr exp) (func->lambda exp)) env)))
  
(define (func->lambda exp)
  (cons 'lambda (cons (cdadr exp) (cons (caddr exp) '()))))

; ---- i f ---- 
; exp is an if statement if it is a list and it begins with 'if
(define (if? exp) (if (pair? exp) (eq? (car exp) 'if) #f))
; if abstraction
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp))) (cadddr exp) 'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
; to evaluate an if, evaluate the predicate
; if the predicate is true evaluate consequent
; if the predicate is false evaluate alternative
(define (true? exp) exp)
(define (false? exp) exp)
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

; ---- c o n d ---- 
; exp is an cond statement if it is a list and it begins with 'cond
(define (cond? exp) (if (pair? exp) (eq? (car exp) 'cond) #f))
; cond abstraction
(define (cond-clauses exp) (cdr exp))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
; to evaluate a cond convert it to an expanded if
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false  ; no else clause
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clauses isn't clast -- COND->IF" clauses))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))
(define (sequence->exp seq)
  (cond
   ((null? seq) seq)
   ((last-exp? seq) (first-exp seq))
   (else (make-begin seq))))

; ---- b e g i n ----
; exp is a sequence if it is a list and it begins with 'begin
(define (begin? exp) (if (pair? exp) (eq? (car exp) 'begin) #f))
; sequence abstration
(define (make-begin seq) (cons 'begin seq))
(define (begin-actions exp) (cdr exp))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (last-exp? seq) (null? (cdr seq)))
; to evaluate a sequence, if we are at the last sequence eval exp 
; else evaluate 
(define (eval-sequence exps env)
  (if (last-exp? exps) 
      (eval (first-exp exps) env)
      (eval-sequence 
       (rest-exps exps) 
       (cons (first-frame (eval (first-exp exps) env)) env))))

; ---- a p p l i c a t i o n ----
; exp is an application if it is a list
(define (application? exp) (pair? exp))
; application abstraction
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (second-operand ops) (cadr ops))
(define (rest-operands ops) (cdr ops))
; to evaluate an application, apply eval'ed operator to eval'ed operands
(define (eval-application exp env) 
  (apply-proc 
   (eval (operator exp) env)
   (evaluate-operands (operands exp) env)
   env))
; recursively evalute the expressions in list exps
(define (evaluate-operands exps env)
  (if (no-operands? exps) '() 
      (cons (eval (first-operand exps) env) 
	    (evaluate-operands (rest-operands exps) env))))


; ---- p r i m i t i v e ----
; exp is a primitive if it is in primitive list
(define (primitive? exp) (contains? (operator exp) primitive-procedures))
(define primitive-procedures 
  (list
   'append
   'car 
   'cdr 
   'cons 
   'list
   '= 
   '+ 
   '- 
   '* 
   '/ 
   '>
   '<
   '>=
   '<=
   'eq? 
   'null?
   'quit))
(define (contains? s los)
  (cond
   ((null? los) #f)
   ((eq? s (car los)) #t)
   (else (contains? s (cdr los)))))
; to evaluate a primitive simply evaluate the operands and 
; dispatch on the operator
(define (eval-primitive exp env)
  (let ((len (length (operands exp))))
    (if (or (= len 0) (= len 1))
	(eval-primitive-uop 
	 (operator exp) 
	 (evaluate-operands (operands exp) env))
	(eval-primitive-bop
	 (operator exp)
	 (evaluate-operands (operands exp) env)))))

(define (eval-primitive-uop rator rands)
  (cond
   ((eq? rator 'length) (length (first-operand rands)))
   ((eq? rator 'null?) (null? (first-operand rands)))
   ((eq? rator 'car) (car (first-operand rands)))
   ((eq? rator 'cdr) (cdr (first-operand rands)))
   ((eq? rator 'random) (random (first-operand rands)))
   ((eq? rator 'list) (list (first-operand rands)))
   ((eq? rator 'quit) 'quit)
   (else (error "Unknown primitive uop -- EVAL-PRIM" rator))))

(define (eval-primitive-bop rator rands)
  (cond
   ((eq? rator '+) (prim-proc-add rands))
   ((eq? rator '-) (prim-proc-sub rands))
   ((eq? rator '*) (prim-proc-mul rands))
   ((eq? rator '=) (prim-proc-equ rands))
   ((eq? rator '<) (prim-proc-ltn rands))
   ((eq? rator '>) (prim-proc-gtn rands))
   ((eq? rator '<=) (prim-proc-lte rands))
   ((eq? rator '>=) (prim-proc-gte rands))
   ((eq? rator 'eq?) (prim-proc-equ rands))
   ((eq? rator 'cons) (prim-proc-con rands))
   ((eq? rator 'append) (prim-proc-app rands))
   ((eq? rator 'list) (prim-proc-lst rands))
   (else 
    (error "Unknown primitive bop -- EVAL-PRIM" rator))))

(define (prim-proc-add rands)
  (if (null? rands) 0 (+ (first-operand rands) (prim-proc-add (rest-operands rands)))))
(define (prim-proc-sub rands)
  (if (null? rands) 0 (- (first-operand rands) (prim-proc-sub (rest-operands rands)))))
(define (prim-proc-mul rands)
  (if (null? rands) 1 (* (first-operand rands) (prim-proc-mul (rest-operands rands)))))
(define (prim-proc-equ rands)
  (eq? (first-operand rands) (second-operand rands)))
(define (prim-proc-ltn rands)
  (< (first-operand rands) (second-operand rands)))
(define (prim-proc-gtn rands)
  (> (first-operand rands) (second-operand rands)))
(define (prim-proc-lte rands)
  (>= (first-operand rands) (second-operand rands)))
(define (prim-proc-gte rands)
  (<= (first-operand rands) (second-operand rands)))
(define (prim-proc-con rands)
  (cons (first-operand rands) (second-operand rands)))
(define (prim-proc-app rands) 
  (prim-proc-app-aux (car rands) (cdr rands)))
(define (prim-proc-app-aux l1 ll2) 
  (if (null? ll2) l1
   (prim-proc-app-aux (append l1 (car ll2)) (cdr ll2))))
(define (append l1 l2) 
  (if (null? l1) l2 (cons (car l1) (append (cdr l1) l2))))




(define (prim-proc-lst rands) rands)
(define (empty-list) '())
; ---- l a m b d a ----
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
; exp is a lambda expression if it is a list and it begins with 'lambda
(define (lambda? exp) (if (pair? exp) (eq? (car exp) 'lambda) #f))
; lambda abstraction
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
; to evaluate a lambda, build a compound-procedure
(define (eval-lambda exp env) (make-procedure (lambda-parameters exp)
					  (lambda-body exp)
					  env))

; ==== a p p l y ====
(define (apply-proc procedure arguments environment)
  (eval-sequence
   (procedure-body procedure)
   (extend-environment
    (procedure-parameters procedure) 
    arguments 
    environment)))

(define (compound-procedure? proc)
  (if (pair? proc) (eq? (car proc) 'procedure) #f))

; c o m p o u n d  p r o c e d u r e

; Read-Eval-Print Loop

(define the-global-environment the-empty-environment)

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (if (let ((input (read)))
	(let ((output (eval input the-global-environment)))
	  (announce-output output-prompt)
	  (display output)
	  (eq? output 'quit)))
      #t ; quit command was issued
      (driver-loop)))
