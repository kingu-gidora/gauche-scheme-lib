(define-module game.dice
  (use srfi-27)
  (export make-dice roll <dice>))

(select-module game.dice)

(random-source-randomize! default-random-source)

(define-class <dice> ()
  ((faces :init-keyword :faces)
   ))

(define make-dice 
  (lambda (f)
    (make <dice> :faces f)))
  
(define-method roll ((self <dice>))
  (+ (random-integer (~ self 'faces)) 1))

(define-method roll ((faces <integer>))
  (roll (make-dice faces)))
