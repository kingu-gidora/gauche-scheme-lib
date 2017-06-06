(define-module jphil.process
  (use gauche.process)
  (export process->list process->string))
(select-module jphil.process)

(define %read-from-port
  (lambda (p)
    (let f ((x (read-line p)))
      (if (eof-object? x) '() (cons x (f (read-line p)))))))

(define process->list
  (lambda (cmd)
    (call-with-input-process
     cmd
     (lambda (p) (%read-from-port p))
     :error :ignore 
     :on-abnormal-exit :ignore
     )))

(define process->string (lambda (c) (string-join (process->list c) "\n")))


