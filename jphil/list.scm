(define-module jphil.list
  (use srfi-1)
  (export-all))

(select-module jphil.list)

(define flatten 
  ;; Flatten a list like:
  ;;  (a (b) (c (d e)((f)))) => (a b c d e f)
  (lambda (L)
    (cond ((null?  L) (list))
	  ((list? (car L))
	   (append (flatten (car L))
		   (flatten (cdr L))))
	  (else (cons (car L) (flatten (cdr L)))))))

(define split-every-nth 
  ;; return a list of list of nth length
  ;; (split-every-nth '(a b c d e f) 2) => ((a b)(c d)(e f))
  (lambda (l nth)
    (if (or (null? l)
	    (<= (length l) nth))
	(list l)		    
	(cons (take l nth)
	      (split-every-nth (drop l nth) nth)))
    ))

(define make-range 
  (lambda (min max)
    (let loop ((n min)) (if (> n max) '() (cons n (loop (+ n 1)))))))

