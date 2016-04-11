(define-module jphil.content-type
  (use file.util)
  (use gauche.parameter)
  (use gauche.process)
  (export content-type-db-file content-type-of))
(select-module jphil.content-type)

(define content-type-db-file (make-parameter "/etc/mime.types"))

(define content-type-of
  (let ((mime-types-hash (make-hash-table))
	(regxp (string->regexp "^([a-zA-Z-]+)/([a-zA-Z-]+)[\s\t\w]+(.+)$")))
    (for-each
     (^[x]
       (let ((m (regxp x))) 
  	 (when m
  	       (let ((exts (string-split (m 3) #\space)))
  		 (for-each
  		  (^[e]
  		    (hash-table-put! mime-types-hash 
				     (string->symbol e)
  				     `(,(m 1) ,(m 2))))
  		  exts)))))
     (file->string-list (content-type-db-file)))
    (lambda (f)
      (or 
       (let ((ext (path-extension f)))
	 (if ext
	     (hash-table-get mime-types-hash 
			     (string->symbol (path-extension f))
			     #f)
	     #f))
       (let* ((info (process-output->string 
		     `(file |-b| |-i| ,f)))
	      (splitted-file-command-result (string-split info #\;)))
	 (string-split 
	  (car splitted-file-command-result) #\/)))
      )))
