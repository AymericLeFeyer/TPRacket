#lang racket


(define M' ((1.2 6 -1.3)(2.5 -1.1 2.9)(3.8 -4 -2.7)))

(define ligne
  (lambda (M i)
    (if (= i 1)
        (car M)
        (ligne (cdr M) (- i 1)))))

(define element
  (lambda (M i j)
    (ligne (ligne M i) j)
    ))

(define colonneReversed
  (lambda (M j n)
    (if (= n 0)
        '()
        (cons (element M n j) (colonneReversed M j (- n 1))))))
   

(define reverse
  (lambda (A)
    (if (null? A)
        '()
        (append (reverse (cdr A)) (list(car A))))))

(define colonne
  (lambda (M j)
    (reverse (colonneReversed M j (length M)))))

(define T
  (lambda (A)
    (Transpo A 1) ))

(define Transpo
  (lambda (A n)
    (if (=  (+ 1 (length A)) n)
        '()
        (cons (colonne A n) (Transpo A (+ n 1))))))
  

    
    