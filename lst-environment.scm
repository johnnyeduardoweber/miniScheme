;-------------------------------------------------
; E n v i r o n m e n t 
;-------------------------------------------------
; We implement environments as a list of FRAMES.
; FRAMES are lists of variable -> value 
; associtations.  
;-------------------------------------------------


; F r a m e   S t r u c t u r e
; Frames are lists of variable -> value 
; associations implemented as two lists:
; a list of variables and a list of the 
; associated values
; ( (var1 var2 var3) val1 val2 val3)

(define (make-empty-frame) '())
(define (empty-frame? frame) (null? frame))
(define (variable-list frame) (car frame))
(define (value-list frame) (cdr frame))

; Frame Constructor
; If the var list and val list are valid
; then build frame 
(define (make-frame vars vals) 
  (if (valid-varval-list? vars vals) (cons vars vals)))
; Check that the var list and val list are valid:
;   a. Check that they are in fact lists
;   b. Check that they are the same size
(define (valid-varval-list? vars vals)
  (if (and (pair? vars) (pair? vals)) 
      (= (length vars) (length vals))))

; Walk the var list and return true if var is 
; the first element in any dotted pair in 
; frame.
(define (defined? var frame) (contains? var (variable-list frame)))
; Return true if a contained in list l
(define (contains? a l)
  (cond
   ((null? l) #f)
   ((eq? a (car l)) #t)
   (else (contains? a (cdr l)))))

; Insert var -> val association in frame
(define (add-binding-to-frame var val frame)
  (cons (cons var (variable-list frame))
  (cons val (value-list frame))))

(define notfound '~)
; Substitute var -> val association in 
; place of the current var -> oldval in frame
(define (set-binding-in-frame var val frame)
  (if (defined? var frame)
      (cons (variable-list frame) 
	(set-binding-in-frame-aux var val (variable-list frame) (value-list frame)))
      frame))
(define (set-binding-in-frame-aux var val varlist vallist) 
  (cond
   ((null? varlist) '())
   ((eq? var (car varlist)) (cons val (cdr vallist)))
   (else (cons (car vallist) 
	       (set-binding-in-frame-aux var val (cdr varlist) (cdr vallist))))))

; Return the val for given var in frame
(define (lkp-binding-in-frame var frame)
  (lkp-binding-in-frame-aux var (variable-list frame) (value-list frame)))
(define (lkp-binding-in-frame-aux var varlist vallist)
  (cond
   ((null? varlist) notfound)
   ((eq? var (car varlist)) (car vallist))
   (else (lkp-binding-in-frame-aux var (cdr varlist) (cdr vallist)))))

; E n v i r o n m e n t   S t r u c t u r e

(define the-empty-environment '())
(define (empty-env? env) (eq? env the-empty-environment))

(define (first-frame env) (car env))
(define (enclosing-environment env) (cdr env))

(define (extend-environment vars vals base-env)
  (cons (make-frame vars vals) base-env))

(define (lookup-variable-value var env)
  (if (empty-env? env) notfound
      (let
	  ((binding (lkp-binding-in-frame var (first-frame env))))
	(if (eq? binding notfound) 
	    (lookup-variable-value var (enclosing-environment env))
	    binding))))

(define (set-variable-value var val env)
  (if (empty-env? env) env
      (let
	  ((frame (set-binding-in-frame var val (first-frame env))))
	(if (defined? var frame)
	    (cons frame (enclosing-environment env))
	    (cons frame (set-variable-value var val (enclosing-environment env)))))))

(define (define-variable var val env)
  (if (empty-env? env) notfound
      (let
	  ((frame (set-binding-in-frame var val (first-frame env))))
	(if (defined? var frame)
	    (cons frame (enclosing-environment env))
	    (cons (add-binding-to-frame var val (first-frame env)) (enclosing-environment env))))))
