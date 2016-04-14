(define-module game.util
  (use srfi-27)
  (use gauche.sequence)
  (use rpg.dice)
  (export-all))
(select-module game.util)

(define-method random-pick ((self <list>))
  (car (shuffle self)))
  
