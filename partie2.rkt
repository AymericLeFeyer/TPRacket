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

(define 
    
    