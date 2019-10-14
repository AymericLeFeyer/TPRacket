#lang racket

(define P '( (5 0)(-3 7)(2 2)(7 9) )  )
(define Ps '( (3 0)(-3 4)(1 2)(3 9) )  )

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

(define sommeMonome
  (lambda(Mo Moo)
    (cons (+ (car Mo) (car Moo)) (cadr Mo))
    )
  )

(define sommeFormelle2
  (lambda(Po Poo Ps i)
    (if(> (degrePolynome Po) (degrePolynome Poo))
       (sommeFormelle3 Po Poo 0 (degrePolynome Po) '() )
       (sommeFormelle3 Po Poo 0 (degrePolynome Poo) '() )
       )
    )
  )



(define sommeFormelle3
  (lambda(Po Poo i nbMax PS)
    (if(= i (+ nbMax 1))
       PS
       (if(= 0 (coefficientMonome (sommeMonome(iemeMonome Po i)(iemeMonome Poo i)) ))
       (sommeFormelle3 Po Poo (+ i 1) nbMax PS)
       (sommeFormelle3 Po Poo (+ i 1) nbMax (cons (sommeMonome(iemeMonome Po i)(iemeMonome Poo i))PS))
                       
       )
    )
  )
  )

(define iemeMonome
  (lambda( Po i)
    ( if(null? Po)
        (list 0 i)
        (if(= (degreMonome (car Po)) i)
           (car Po)
           (iemeMonome (cdr Po) i)
           )
        )
    )
  )
 

    
    

