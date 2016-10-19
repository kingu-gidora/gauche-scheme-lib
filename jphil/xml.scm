(define-module jphil.xml
  (use sxml.ssax)
  (use sxml.sxpath)
  (export-all))

(select-module jphil.xml)


(define xml-file->sxml 
  (lambda (xmlf) 
    (call-with-input-file xmlf
      (lambda (p)
	(ssax:xml->sxml p '())))))
  
(define find-by-path (lambda (path tree :optional (default #f)) 
		       (let* ((strpath (string-append "/" (string-join (map x->string path) "/")))
			      (data ((sxpath strpath) tree))) 
			 ;;(print data)
			 
			 (if (or (null? data)
				 (null? (cdar data))) default (cadar data))))) 



