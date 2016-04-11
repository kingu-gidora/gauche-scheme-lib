(define-module jphil.stack
  (export-all))
(select-module jphil.stack)

(define-class <stack> ()
  ((stack :getter get-stack :setter set-stack! :init-value '())))

(define-method stack? ((self <stack>)) #t)
(define-method stack? ((self <top>)) #f)

(define-method flush! ((self <stack>))
  (set-stack! self '()))

(define-method empty? ((self <stack>))
  (null? (get-stack self)))

(define-method stack! ((self <stack>) value)
  (let ((stk (get-stack self)))
    (set-stack! self (cons value stk))
    value))

(define-method unstack! ((self <stack>))
  (if (empty? self)
      #f
      (let ((v (stack-top self)))
	(set-stack! self (cdr (get-stack self)))
	v)))

(define-method stack-top ((self <stack>))
  (if (empty? self)
      #f
      (car (get-stack self))))
  	
(define-method stack-length ((self <stack>))
  (length (get-stack self)))

(define-method stack->list ((self <stack>))
  (get-stack self))

(define-method list->stack ((l <list>))
  (let ((stk (make <stack>)))
    (set-stack! stk l)
    stk))

(define-method stack->string ((self <stack>))
  (call-with-output-string
    (lambda (port)
      (for-each
       (lambda (x)
	 (format port "[~s]" x))
       (get-stack self)))))
    

(define-method object-apply ((self <stack>))
  (unstack! self))
(define-method object-apply ((self <stack>)(value <top>))
  (stack! self value))

(define-method oper1! ((self <stack>)(f <procedure>))
  (if (empty? self) 
      #f
      (let1 v (self)
	(self (f v)))))
(define-method oper2! ((self <stack>)(f <procedure>))
  (cond ((empty? self) #f)
	((> (stack-length self) 1)
	 (let ((v2 (self))
	       (v1 (self)))
	   (self (f v1 v2))))
	(else
	 (let ((v (self)))
	   (self (f v))))))

(define-method +! ((self <stack>)) (oper2! self +)) 
(define-method -! ((self <stack>)) (oper2! self -))
(define-method /! ((self <stack>)) (oper2! self /))
(define-method *! ((self <stack>)) (oper2! self *))
(define-method sqrt! ((self <stack>)) (oper1! self sqrt))  
(define-method abs! ((self <stack>)) (oper1! self abs))  
(define-method dup! ((self <stack>)) (unless (empty? self)(self (stack-top self))))
(define-method drop! ((self <stack>)) (self))
(define-method swap! ((self <stack>)) (when (> (stack-length self) 1)
					(let ((t (self)) 
					      (bt (self)))
					  (self t)(self bt))))
(provide "stack")
