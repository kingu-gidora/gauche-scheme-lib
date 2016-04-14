(define-module game.astar-heap
  (use game.grid)
  (use jphil.binheap)
  (export-all))
(select-module game.astar-heap)

(define %neighbours-diag
  ;; Return node neighbours (include diagonals) 
  (lambda (node)
    (let ((x (car node))(y (cdr node)))
      `((,(- x 1) . ,(- y 1))(,x . ,(- y 1))
	(,(+ x 1) . ,(- y 1))(,(- x 1) . ,y)
	(,(+ x 1) . ,y)(,(- x 1) . ,(+ y 1))
	(,x . ,(+ y 1))(,(+ x 1) . ,(+ y 1))))))

(define %neighbours-no-diag
  ;; Return node neighbours (exclude diagonals)
  (lambda (node)
    (let ((x (car node))(y (cdr node)))
      `((,x . ,(- y 1))
	(,(- x 1) . ,y)
	(,(+ x 1) . ,y)
	(,x . ,(+ y 1))
	))))
    
(define %neighbours-hex
  ;; Return node neighbours (hex) 
  (lambda (node)
    (let ((x (car node))(y (cdr node)))
      `(,(if (even? x) `(,(- x 1) . (- y 1)) `(,(- x 1) . y)) ; nw
	(,x . ,(- y 1)) ; n
	,(if (even? x) `(,(+ x 1) . ,(- y 1)) `(,(+ x 1) . y)) ; ne
	,(if (even? x) `(,(+ x 1) . y) `(,(+ x 1) . ,(+ y 1))) ; se
	(,x . ,(+ y 1))       ; s
	,(if (even? cx) `(,(- x 1) . y)) ; sw
))))

(define get-neighbours-func
  (lambda (node grid diag?)
    (letrec ((loop (lambda (nn) 
		     (if (null? nn)
			 '()
			 (if (or (< (caar nn) 0) (< (cdar nn) 0)
				 (> (caar nn) (- (grid 'width) 1)) 
				 (> (cdar nn) (- (grid 'height) 1)))
			     (loop (cdr nn))
			     (cons (car nn) (loop (cdr nn))))))))
      (loop ((if diag? %neighbours-diag %neighbours-no-diag) node)))))

(define get-neighbours-iter
  (lambda (node grid-width grid-height)
    (let ((nb '()))
      (for-each
       (lambda (nn)
	 (unless (or (< (car nn) 0) 
		     (< (cdr nn) 0)
		     (> (car nn) (- (grid 'width) 1)) 
		     (> (cdr nn) (- (grid 'height) 1)))
		 (push! nb nn)))
       (%neighbours-diag node))
      nb)))

(define get-neighbours-jps
  (lambda (node grid diag?)
    (letrec ((loop (lambda (nn) 
		     (if (null? nn)
			 '()
			 (if (or (< (caar nn) 0) (< (cdar nn) 0)
				 (> (caar nn) (- (grid 'width) 1)) 
				 (> (cdar nn) (- (grid 'height) 1))
				 )
			     (loop (cdr nn))
			     (cons (car nn) (loop (cdr nn))))))))
      (loop ((if diag? %neighbours-diag %neighbours-no-diag) node)))))


(define get-neighbours-hex
  (lambda (node grid-width grid-height)
    (letrec ((loop (lambda (nn) 
		     (if (null? nn)
			 '()
			 (if (or (< (caar nn) 0) (< (cdar nn) 0)
				 (> (caar nn) (- grid-width 1)) 
				 (> (cdar nn) (- grid-height 1)))
			     (loop (cdr nn))
			     (cons (car nn) (loop (cdr nn))))))))
      (loop (%neighbours-hex node)))))
  
     
;; Heuristics

(define manhattan
  (lambda (from to move-value)
    (* (+ (abs (- (car from) (car to)))
	  (abs (- (cdr from) (cdr to))))
       move-value))) 

(define euclidian
  (lambda (from to move-value)
    (let ((sq (lambda (zz) (* zz zz))))
      (* (sqrt (+ (sq (- (car from) (car to)))
		  (sq (- (cdr from) (cdr to)))))
	 move-value))))

(define dijkstra
  (lambda (from to move-value) 0))
  
(define move-cost
  (lambda (n1 n2)
    (if (or (= (car n1) (car n2))
	    (= (cdr n1) (cdr n2)))
	10 14)))

(define walkable?
  ;; 0 = walled 1+=walkable
  (lambda (grid n)
    (let ((w? (>= (grid-ref grid (car n)(cdr n)) 1)))
;      (format #t "~a: ~a ~a\n" n (grid-ref grid (car n)(cdr n)) w?)
      w?)))

(define node-iso-coor
  (lambda (n)
    `(,(- (car n) (cdr n)) . ,(/. (+ (car n)(cdr n)) 2))))
      
(define a*
  (lambda (grid from to . options ) 
    (let-keywords options ((neighbours (lambda (n) (get-neighbours-func n grid #f)))
			   (heuristic manhattan)
			   (move move-cost)
			   (walkable (lambda (n) (walkable? grid n))))
		  (letrec ((nodes (make-grid (grid 'width) (grid 'height) (make-vector 4 0)))
			   (openL (make-binheap '() (lambda (a b) (< (get-F a)(get-F b))))) 
			   (closeL '())
			   (x car)(y cdr)
			   (unsolvable? (lambda (n) 
					  (and (= (binheap-size openL) 0) 
					       (not (equal? n from)))))
			   (in-open? (lambda (n) (member n (vector->list 
							    (binheap-vect openL)))))
			   (in-close? (lambda (n) (member n closeL)))

			   (add-to-openL! (lambda (n pn)
					    ;;(format #f "Adding ~a to openL\n" n)
					    (unless (in-open? n)
						    (set-GHFP! n pn)
						    (binheap-insert! openL n)
						    )))
			   
			   (add-to-closeL! (lambda (n)
					     ;; (set! openL (remove (lambda (zz) 
					     ;; 			   (equal? zz n)) 
					     ;; 			 openL))
					     (push! closeL n)))
			   
			   (get-G (lambda (n) (vector-ref (grid-ref nodes (car n) (cdr n)) 0)))
			   (get-H (lambda (n) (vector-ref (grid-ref nodes (car n) (cdr n)) 1)))
			   (get-F (lambda (n) (+ (get-G n) (get-H n))))
			   
			   (new-G (lambda (n pn) (+ (get-G pn) (move n pn))))
			   (new-H (lambda (n) (heuristic n to 10)))
			   
			   (set-GHFP! (lambda (n pn) 
					(let* ((G (new-G n pn))
					       (H (new-H n))
					       (F (+ G H)))
					  (grid-set! nodes (car n)(cdr n)(vector G H F pn)))))
			   
			   (reorder-openL! (lambda ()
					     (set! openL (sort openL (lambda (n1 n2)
								       (< (get-F n1)
									  (get-F n2)))))))
			   

			   (search (lambda (cn parent)
				     (add-to-closeL! cn)
				     (if (equal? cn to) 
					 (begin
					   (set-GHFP! cn parent)
					   (letrec ((rebuild-path 
						     (lambda (n)
						       (if (equal? n from)
							   '()
							   (cons n (rebuild-path 
								    (vector-ref  
								     (grid-ref nodes (car n) (cdr n)) 3)))))))
					     (cons from (reverse (cons to (rebuild-path parent))))))
					 (begin
					   (for-each 
					    (lambda (n)
					      (when (and (walkable n)
							 (not (in-close? n)))
						    (when (> (get-F n) (+ (new-G n cn)(new-H n))) (set-GHFP! n cn))
						    (add-to-openL! n cn)
						    ))
					    (neighbours cn))
					   (if (null? openL)
					       'no-path-found
					       (let ((ncn (binheap-extract-root! openL)))
						 (search ncn cn))))))))
		    
		    (search from #f)))))



