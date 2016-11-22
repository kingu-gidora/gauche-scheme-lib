(define-module jphil.time
  (use srfi-19) 
  (export-all))
(select-module jphil.time)

(define-method decompose-seconds ((seconds <integer>) (format-string <string>))
  (let* ((sec (mod seconds 60))
	 (min (round (div (mod total-seconds 3600) 60)))
	 (hours (round (div (mod total-seconds 86400) 3600)))
	 (days (round (div total-seconds (* 60 (* 60 24))))))
    (format #f format-string days hours min sec))) 

(define-method decompose-seconds ((seconds <integer>)) 
  (decompose-seconds seconds "~a days ~a hours ~a minutes & ~a seconds\n" ))
