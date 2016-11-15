(define-module jphil.csv
  (use text.csv)
  (export csv-for-each))

(select-module jphil.csv)

;; csv-foreach 
;;   csv-file <string>     - The csv file to parse
;;   func     <procedure>  - The procedure to invoque for each line
;;  KEYS:
;;   :before  <trunk>  - A procedure to invoque before the parsing
;;   :after   <trunk>  - A procedure to invoque after the parsing
;;   :skip-first-line? - If #t the first line of the file will be ignored (default #f)

;; (define csv-for-each  
;;   (lambda (csv-file func :key (before #f)(after #f)(skip-first-line? #f))
;;     (call-with-input-file csv-file
;;       (lambda (p)
;; 	(let ((csv-reader (make-csv-reader #\,)))
;; 	  (when skip-first-line? (csv-reader p))
;; 	  (dynamic-wind
;; 	      (lambda () (when before (before)))
;; 	      (lambda () (let ((x (csv-reader p)))
;; 			   (until (eof-object? x)
;; 				  (func x)
;; 				  (set! x (csv-reader p)))))
;; 	      (lambda () (when after (after)))))))))

(define csv-for-each  
  (lambda (csv-file func :key (before #f)(after #f)(skip-first-line? #f))
    (call-with-input-file csv-file
      (lambda (p)
	(let ((csv-reader (make-csv-reader #\,)))
	  (when skip-first-line? (csv-reader p))
	  (let ((x (csv-reader p)))
	    (until (eof-object? x)
		   (when before (before))
		   (func x)
		   (when after (after))
		   (set! x (csv-reader p)))))))))

;; (define csv->html
;;   (lambda (csv-file)
;;     (csv-for-each  csv-file (lambda (x)
;; 			      (format #f "<td>~a</td>" x)
     
     

