(define-module jphil.type-convert
  (export-all))
(select-module jphil.type-convert)
  
(define-method ->string ((x <top>)) (x->string x))
(define-method ->symbol ((x <top>)) (string->symbol (x->string x)))
(define-method ->number ((x <top>)) (or (string->number (x->string x)) 0))
(define-method ->number ((x <boolean>)) (if x 1 0))



