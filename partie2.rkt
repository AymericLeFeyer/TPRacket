#lang racket


(define M' ((1.2 6 -1.3)
            (2.5 -1.1 2.9)
            (3.8 -4 -2.7)))

(define N' ((1 2 3)
            (4 5 6)
            (7 8 9)))

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
    (transpo A 1) ))

(define transpo
  (lambda (A n)
    (if (=  (+ 1 (length A)) n)
        '()
        (cons (colonne A n) (transpo A (+ n 1))))))

(define concat
  (lambda (A B)
    (append A B)))

(define infixe
  (lambda (M)
    (infixe2 M (length M))))

(define infixe2
  (lambda (M n)
    (if (= 0 n)
        '()
        (concat (ligne M n) (infixe2 M (- n 1))))))

(define estSymetrique?
  (lambda (M)
    (estSymetrique2? (infixe (T M)) (infixe M) (length (infixe M)))))

(define estSymetrique2?
  (lambda (M Tr n)
    (if (= n 0)
        #t
        (if (= (car M) (car Tr))
            (estSymetrique2? (cdr M) (cdr Tr) (- n 1))
            #f))))

(define sommeListe
  (lambda (A B)
    (if (= (length A) (length B))
        (if (null? A)
            '()
            (concat (list(+ (car A) (car B))) (sommeListe (cdr A) (cdr B) )))
        ("erreur"))))

(define sommeMatrice
  (lambda (A B)
    (sommeMatrice2 A B 1)))

(define sommeMatrice2
  (lambda (A B n)
    (if (= n (+ 1 (length A)))
        '()
        (concat (list(sommeListe (ligne A n) (ligne B n))) (sommeMatrice2 A B (+ n 1))))))

(define produitLigne
  (lambda (A B)
    (if (null? A)
        0
        (+ (* (car A) (car B)) (produitLigne (cdr A) (cdr B))))))

(define produit
  (lambda (A B)
    (produit2 A B 1 1)))

(define produit2
  (lambda (A B i j)
    (if (= i (+ 1 (length A)))
        '()
        (if (= j (+ 1 (length B)))
            (list(concat (list(produitLigne (ligne A i) (colonne B j))) (produit2 A B (+ i 1) 1)))
            (concat (list(produitLigne (ligne A i) (colonne B j))) (produit2 A B i (+ j 1)))))))
            
            
        
        
        
    
    

    
    
        
    




  

    
    