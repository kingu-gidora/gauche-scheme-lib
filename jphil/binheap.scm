(define-module jphil.binheap
  (export-all))
(select-module jphil.binheap)

(define binheap-size 
  (lambda (binheap)
    (vector-ref binheap 2)))

(define binheap-vect 
  (lambda (binheap)
    (vector-ref binheap 1)))

(define binheap-read 
  (lambda (binheap)
    (vector-ref (vector-ref binheap 1) 0)))

(define binheap<? 
  (lambda (binheap index1 index2)
    (let ((vect (binheap-vect binheap)))
      ((vector-ref binheap 0) 
       (vector-ref vect index1) 
       (vector-ref vect index2)))))

(define binheap-swap! 
  (lambda (binheap index1 index2) 
    (let* ((vect (binheap-vect binheap))
	   (val1 (vector-ref vect index1)))
      (begin
	(vector-set! vect index1 (vector-ref vect index2))
	(vector-set! vect index2 val1)))))

(define vector-resize 
  (lambda (vect new-size)
    (define overwrite-vector 
      (lambda (alist avector position)
	(if (null? alist) avector
	    (begin
	      (vector-set! avector position (car alist))
	      (overwrite-vector (cdr alist) avector (+ position 1))))))
    (overwrite-vector (vector->list vect) (make-vector new-size) 0)))

(define binheap-move-up 
  (lambda (binheap position)
    (let ((parent (quotient (- position 1) 2)))
      (cond ((or (= position 0) (binheap<? binheap parent position))  
             'end-move-up)
            ((binheap<? binheap position parent)
             (begin
               (binheap-swap! binheap position parent)
               (binheap-move-up binheap parent)))
            (else 'end-move-up)))))

(define binheap-insert! 
  (lambda (binheap elem)
    (let ((size (binheap-size binheap)))
      (begin 
	(if (> (+ size 1) (vector-length (vector-ref binheap 1)))
	    (vector-set! binheap 1 (vector-resize (vector-ref binheap 1) (* 2 (+ 1 size)))))
	(vector-set! (vector-ref binheap 1) size elem)
	(vector-set! binheap 2 (+ size 1))
	(binheap-move-up binheap size)))))

(define binheap-move-down 
  (lambda (binheap position)
    (let ((lchild (+ (* 2 position) 1))
          (rchild (+ (* 2 position) 2))
          (size (binheap-size binheap))
          (swap! (lambda (pos child)
                   (begin
                     (binheap-swap! binheap pos child)
                     (binheap-move-down binheap child)))))
      (cond ((> lchild (- size 1))
             'end-move-down)
            ((> rchild (- size 1))
             (if (binheap<? binheap lchild position)
                 (swap! position lchild)
                 'end-move-down))
	    (else
	     (let ((smallest-child (if (binheap<? binheap lchild rchild) lchild rchild)))
	       (if (binheap<? binheap smallest-child position)
		   (swap! position smallest-child)
		   'end-move-down)))))))

(define binheap-extract-root! 
  (lambda (binheap)
    (let ((size (binheap-size binheap)) (old-root (binheap-read binheap)))
      (begin
	(binheap-swap! binheap 0 (- size 1))
	(vector-set! binheap 2 (- size 1))
	(binheap-move-down binheap 0)
	old-root))))

(define make-binheap 
  (lambda (elem-list lt-predicate)
    (define (binheap-fill binheap alist)
      (if (null? alist) binheap
	  (begin
	    (binheap-insert! binheap (car alist))
	    (binheap-fill binheap (cdr alist)))))
    (let ((new-binheap (make-vector 3)))
      (begin
	(vector-set! new-binheap 0 lt-predicate)
	(vector-set! new-binheap 1 (make-vector 0))
	(vector-set! new-binheap 2 0)
	(binheap-fill new-binheap elem-list)))))

(define binheap-sort 
  (lambda (alist apred)
    (define extract-until 
      (lambda (abinheap)
	(if (= 0 (binheap-size abinheap)) '()
	    (cons (binheap-extract-root! abinheap) (extract-until abinheap)))))
    (extract-until (make-binheap alist apred))))

