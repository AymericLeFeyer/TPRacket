#lang racket

(define P '( (5 0)(-3 7)(2 2)(7 9) )  )

(define degreMonome
  (lambda(M)
    (cadr M)
    )
  )

(define coefficientMonome
  (lambda(M)
    (car M)
    )
  )



(define degrePolynome
  (lambda(Po)
    (degrePolynome2 Po 0)
    )
  )

(define degrePolynome2
  (lambda(Po D)
    (if(null? Po)
       D
       (if(> (degreMonome (car Po)) D)
          (degrePolynome2 (cdr Po) (degreMonome (car Po)))
          (degrePolynome2 (cdr Po) D)
          )
       )
    )
  )

(define valeurMonome
  (lambda(M V)
    (* (expt V (degreMonome M)) (coefficientMonome M))
    )
  )



(define valeurPolynome
  (lambda(Po V)
    (valeurPolynome2 Po V 0)
    )
  )

(define valeurPolynome2
  (lambda(Po V T) 
    (if(null? Po)
       T
       (+ (valeurMonome (car Po) V) (valeurPolynome2 (cdr Po) V T))
       )
    )
  )