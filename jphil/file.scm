(define-module jphil.file
  (use jphil.list)
  (export-all))
(select-module jphil.file)

(define (sanitize-path key)  
  ;; taken from fsdbm.scm (Shiro Kawai)
  (with-string-io key
    (lambda ()
      (write-char #\_) ;; always emit one char, so we can deal with null key                
      (let loop ((c (read-byte))
                 (count 0))
	(cond [(eof-object? c)]
              [(>= count 255)
               (write-char #\/) (loop c 0)]
              [(and (< c 127)
                    (char-set-contains? #[0-9a-zA-Z] (integer->char c)))
	              (write-byte c) (loop (read-byte) (+ count 1))]
              [else
               (format #t "~2,'0X" c)
               (loop (read-byte) (+ count 1))])))))


;; return all regular files under DIR, including subdirectories content
;; (might be faster to use glob)
(define recursive-directory-list
  (lambda (dir)
    (flatten
     (map
      (lambda (f)
	(if (file-is-directory? f)
	    (recursive-directory-list f)
	    f))
      (directory-list dir 
		      :add-path? #t
		      :children? #t)))))

(define string->file
  (lambda (string file) (call-with-output-file file (^[p] (display string p))) file))

(define-method file-for-each ((proc  <procedure>)(file <string>))
  (call-with-input-file file
    (lambda (p) 
      (let loop ((line (read-line p)))
	(unless (eof-object? line)	
		(proc line)
		(loop (read-line p)))))))
