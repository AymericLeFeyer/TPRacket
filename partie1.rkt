#lang racket

(define aireRectangle
  (lambda (w h)
    (* h w)))
  

(define f
  (lambda (x)
    (* x x x)))

(define milieu
  (lambda (k a b n)
    (+ a (* (- k 1) (/ (- b a) n)) (/ (- b a) (* 2 n)))))

(define integrale
  (lambda (f n a b)
    (calculIntegrale f n a b n )))

(define calculIntegrale
  (lambda (f n a b i)
    (if (= i 0)
        0
        (+ (aireRectangle (/ (- b a) n) (f (milieu i a b n))) (calculIntegrale f n a b (- i 1))))))
    
    

    

     