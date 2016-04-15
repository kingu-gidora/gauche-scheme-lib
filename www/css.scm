(define-module www.css
  (export css css-rules stylesheet))
(select-module www.css)

;; Take a list of css RULES in scheme notation and return them with css syntax 
;; ex: (css '((color blue)(width "50px")) ==> "color:blue;width:50px"
(define css  
  (lambda (properties 
	   :key (port #f)
	        (human-readable #f))
    (format port "~a"
	    (string-join 
	     (map (^[x] 
		    (string-join 
		     (map x->string x) ":")) 
		  properties) 
	     ";"))))

(define css-rule 
  (lambda (selectors properties :key (port #f)(human-readable #f))
    (format #f "~a { ~a }\n"
	    (cond ((string? selectors) selectors)
		  ((symbol? selectors) (x->string selectors))
		  ((list? selectors) (string-join (map x->string selectors) ", ")))
	    (css properties))))
     
(define stylesheet 
  ;; wrapper function to include many css-rules 
  ;; (stylesheet '((selectors properties) ... ))
  (lambda (rules :key (port #f)(human-readable #f))  
    (map (^[r] (css-rule (car r)(cadr r))) rules)))
