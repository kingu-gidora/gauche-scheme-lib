(define-module jphil.date
  (use srfi-19)
  (export-all))
(select-module jphil.date)

;; Check if the date is valid and had or will exist
(define-method date-exists? ((date <date>))
  (let ((M (date-month date))
	(div? (lambda (x) (zero? (mod (date-year date) x)))))
    (not (or (< M 1) (> M 12) 
	     (> (date-day date) 
		(~ (vector 31 
			   (if (or (div? 400) (and (div? 4) (not (div? 100)))) 29 28)
			   31 30 31 30 31 31 30 31 30 31)
		   (- M 1)))))))

(define-method previous-day ((date <date>))
  (let* ((y (date-year date))
	 (m (date-month date))
	 (d (date-day date))
	 (d- (make-date 0 0 0 0 (- d 1) m y 0)))
    (if (date-exists? d-) 
	d-
	(let ((m- (make-date 0 0 0 0 1 (- m 1) y 0)))
	  (if (date-exists? m-)
	      m-
	      (let ((y- (make-date 0 0 0 0 1 1 (- y 1) 0)))
		(if (date-exists? y-)
		    y-
		    (error "something is wrong!"))))))))
  

(define-method next-day ((date <date>))
  (let* ((y (date-year date))
	 (m (date-month date))
	 (d (date-day date))
	 (d+ (make-date 0 0 0 0 (+ d 1) m y 0)))
    (if (date-exists? d+) 
	d+
	(let ((m+ (make-date 0 0 0 0 1 (+ m 1) y 0)))
	  (if (date-exists? m+)
	      m+
	      (let ((y+ (make-date 0 0 0 0 1 1 (+ y 1) 0)))
		(if (date-exists? y+)
		    y+
		    (error "something is wrong!"))))))))
	      
(define-method same-date? ((datea  <date>)(dateb <date>))
  (and (= (date-year datea)(date-year dateb))
       (= (date-month datea)(date-month dateb))
       (= (date-day datea)(date-day dateb))))
  
(define-method date-range ((from <date>)(to <date>))
  (letrec ((loop (lambda (d res) 
		   (if (same-date? d to)
		       (reverse (cons d res))
		       (loop (next-day d) (cons d res))))))
    (loop from '())))
	 
    
(define yesterday 
  (lambda () (previous-day (current-date))))

(define tomorrow 
  (lambda ()
    (next-day (current-date))))

      
  
  

