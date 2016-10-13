(define-module jphil.pprint
  (export tree->string pprint))

;; From code taken at: https://practical-scheme.net/wiliki/wiliki.cgi?Gauche%3APrettyPrint

(define tree->string 
  (lambda (tree)
    (letrec ((indent 1)
	     (insert-space (lambda (n) (display (make-string n #\space))))
	     (format-one-list (lambda (t n)
				(display "(")
				(let loop ((l t))
				  (cond ((null? l) (display ")"))
					((list? (car l))
					 (format-one-list (car l) (+ n indent))
					 (unless (null? (cdr l)) (newline) (insert-space n))
					 (loop (cdr l)))
					(else
					 (write (car l))
					 (cond ((null? (cdr l)))
					       ((list? (cadr l)) (newline) (insert-space n))
					       (else (display " ")))
					 (loop (cdr l))))))))
      (with-output-to-string (cut format-one-list tree indent)))))

(define pprint (lambda (tree) (print (tree->string tree)))) 

