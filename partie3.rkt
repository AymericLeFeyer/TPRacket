#lang racket


(define P '( (5 0)(-3 7)(2 2)(7 9) )  )
(define Ps '( (3 0)(-3 4)(1 2)(3 9) )  )
(define Pss '( (4 1)(-21 6)(63 8) )  )


(define Psss '( (7 3)(9 2)(1 1) (5 0))  )
(define Pssss '( (2 2) (4 1) (2 0))  )



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
    (list (+ (car Mo) (car Moo)) (cadr Mo))
    )
  )


(define sommeFormelle
  (lambda(Po Poo)
    (if(null? Po)
       Poo
    (sommeFormelle2 Po Poo '())
    )
  )
  )

(define sommeFormelle2
  (lambda(Po Poo Ps)
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
       (sommeFormelle3 Po Poo (+ i 1) nbMax (append PS (list(sommeMonome(iemeMonome Po i)(iemeMonome Poo i)))))
                       
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

(define derive
  (lambda (P)
    (deriveFormelle P '())))

(define deriveFormelle
  (lambda(Po Poo)
    (if(null? Po)
       Poo
       (if(> (degreMonome (car Po)) 1)
          (deriveFormelle (cdr Po) (append Poo (list(list (* (degreMonome (car Po)) (coefficientMonome (car Po))   ) (- (degreMonome (car Po)) 1) ) )))

          (deriveFormelle (cdr Po)  Poo)
          )
       )
    )
  )

(define primitive
  (lambda (P)
    (primitiveFormelle P '())))

(define primitiveFormelle
  (lambda(Po Poo)
    (if(null? Po)
       Poo
        (primitiveFormelle (cdr Po) (append Poo (list(list (/ (coefficientMonome (car Po)) (+ 1 (degreMonome (car Po)))) (+ (degreMonome (car Po)) 1) ))))
       )
    )
  )

(define multMoParPo
  (lambda(Mo Po Ps)
    (if(null? Po)
       Ps
       (multMoParPo Mo (cdr Po) (append Ps (list (list(* (coefficientMonome Mo) (coefficientMonome (car Po))   ) (+ (degreMonome (car Po)) (degreMonome Mo)) ) )))
       )
    )
  )

(define multiplication
  (lambda (P Ps)
    (multiplicationFormelle P Ps '())))

(define multiplicationFormelle
  (lambda(Po Poo Ps)
    (if(null? Po)
       Ps
       (multiplicationFormelle (cdr Po) Poo (sommeFormelle Ps (multMoParPo (car Po) Poo '())))
       )
    )
  )