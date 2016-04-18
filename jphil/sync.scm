(define-module jphil.sync
  (use gauche.process)                                                                                 
  (export sync sync-both-ways))
(select-module jphil.sync)

(define sync
  (lambda (source target)
;    (unless (file-exists? source) (error "Source file does not exists!"))
;    (unless (file-exists? target) (error "Target file does not exists!"))
    (let ((cmd `(rsync -rtu ,source ,target)))
      (run-process cmd :wait #t))))
;(define rsync sync) 

(define sync-both-ways
  (lambda (dir1 dir2)
    (sync dir1 dir2)
    (sync dir2 dir1)))


