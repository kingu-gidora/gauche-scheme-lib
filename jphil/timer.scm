(define-module jphil.timer
  (use srfi-19) 
  (export with-timer))
(select-module jphil.timer)

;; with-timer: execute THUNK and time the execution
;; if OUTPUT is set, a message is printed to this port.
;; If VALUE is set, the proc return 2 values time and the regular return value
;; if RETURN-TIMER? is set, with-timer return the time and ignore THUNK result
(define with-timer
  (let ((start-time 0))
    (lambda (thunk :key
		   (output #t)
		   (values? #f)
		   (format-string "Time: ~a\n")
		   (return-timer? #f))
      (set! start-time (current-time))
      (let ((result (thunk)))
	(let* ((_diff (time-difference (current-time) start-time))
	       (diff (string->number (format #f "~a.~a" (~ _diff 'second) (~ _diff 'nanosecond))))) 
	  (when output (format output format-string diff))
	  
	  (cond (return-timer? diff) 
		(values? (values diff result))
		(else result)))))))
	
