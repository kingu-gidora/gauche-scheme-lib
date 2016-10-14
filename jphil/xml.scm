(define-module jphil.xml
  (use sxml.ssax)
  (export-all))

(select-module jphil.xml)


(define xml-file->sxml 
  (lambda (xmlf) 
    (call-with-input-file xmlf
      (lambda (p)
	(ssax:xml->sxml p '())))))



  



