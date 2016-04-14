(define-module game.grid
  (use jphil.list)
  (use gauche.sequence)
  (export-all))
(select-module game.grid)

(define make-grid
  (lambda (width height fill)
    (let* ((g (make-vector (* width height) fill))
	   (idx (lambda (x y) (+ (* width y) x))))
      (lambda (req . args)
	(case req
	  ((get) (vector-ref g (idx (car args) (cadr args))))
	  ((set) (vector-set! g (idx (car args) (cadr args)) 
			       (caddr args)))
	  ((width) width)
	  ((height) height)
	  ((vector) g)
	  )))))

(define grid-ref  (lambda (g x y) (g 'get x y)))

(define grid-set! (lambda (g x y v) (g 'set x y v)))
    
(define grid-for-each
  (lambda (g eachf bolf eolf)
    (let ((sl (split-every-nth (vector->list (g 'vector)) (g 'width))))
      (for-each
       (^[y]
	 (bolf y)
	 (for-each (^[x] (eachf x)) y)
	 (eolf y)
	 )
       sl))))

(define grid-for-each-with-index
  (lambda (g eachf bolf eolf)
    (let ((sl (split-every-nth (vector->list (g 'vector)) (g 'width))))
      (for-each-with-index
       (^[y y-val]
	 (when bolf (bolf y y-val))
	 (for-each-with-index (^[x x-val] (eachf x y x-val)) y-val)
	 (when eolf (eolf y y-val))
	 )
       sl))))

(define grid-copy 
  (lambda (g)
    (let ((Grd (make-grid (g 'width) (g 'height) 0)))
      ;;(grid-display Grd)
      (grid-for-each-with-index g (lambda (x y zz) 
				    ;;(format #t "~a-~a: ~a\n" x y (grid-ref g x y))
				    (Grd 'set x y (grid-ref g x y)))
				#f #f)
      ;;(grid-display Grd)
      Grd)))

(define grid-display 
  (lambda (g)
    (grid-for-each g 
		   (lambda (x) (format #t "~2@a " x))
		   (lambda (y) #t)
		   (lambda (y) (newline)))))
     

		 
(define vov->grid
  (lambda (vov)
    (let* ((ly (vector-length vov))
	   (lx (vector-length (vector-ref vov 1)))
	   (G (make-grid lx ly #f)))      
      (vector-for-each-with-index
       (lambda (y line)
	 (vector-for-each-with-index 
	  (lambda (x val)
	    (grid-set! G x y val))
	  line))
       vov)
      G)))
      
