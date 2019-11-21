#lang racket

; Partie 1

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

; Partie 2

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

(define produitLigneLigne
  (lambda (A B)
    (if (null? A)
        0
        (+ (* (car A) (car B)) (produitLigneLigne (cdr A) (cdr B))))))

(define produitLigneMatrice
  (lambda (A B)
    (produitLigneMatrice2 A B 1 1)))

(define produitLigneMatrice2
  (lambda (A B i j)
    (if (< (length B) j)
        '()
        (cons (list(produitLigneLigne A (colonne B j))) (produitLigneMatrice2 A B i (+ j 1))))))


(define produit
  (lambda (A B)
    (produit2 A B 1 1)))

(define produit2
  (lambda (A B i j)
    (if (< (length A) i)
        '()
        (cons (produitLigneMatrice (ligne A i) B) (produit2 A B (+ i 1) j)))))


  


(define estSymetrique4?
  (lambda (M T I J)
    (if(= (element M I J) (element T I J) )
       (if(= J (- (length M) 0) )
          #t
          (if(= I (- (length M) 0) )
             (estSymetrique4? M T 1 (+ J 1) )
             (estSymetrique4? M T (+ I 1) J)
             )
          )
       #f)))

; Partie 3

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

; Partie 4

(define L' ((("Helin" "Dylan" "29/10/1999" 2) (60 "rue Waldeck Rousseaux" 59410 "Anzin" "France") "0652274310")
            (("Helin" "Steven" "30/10/1999" 1) (60 "rue Waldeck Rousseaux" 59410 "Anzin" "France") "0652274312")
            (("Test" "Steven" "30/10/1999" 8) (60 "rue Waldeck e" 59410 "Anzien" "France") "0652274512")
            (("Baudelet" "Conrad" "31/10/1999" 3) (23 "rue Test" 59345 "Escaudin" "France") "0652274513")))

(define Aymeric' (("Le Feyer" "Aymeric" "29/10/1999" 4) (60 "rue Waldeck Rousseaux" 59410 "Anzin" "France") "0652274310"))
(define Conrad' (("Baudelet" "Conrad" "29/10/1999" 3) (60 "rue Waldeck Rousseaux" 59410 "Anzin" "France") "0652274310"))


(define creationAdresse
  (lambda (Numero Rue CP Ville Pays)
    (cons Numero (cons Rue (cons CP (cons Ville Pays))))))

(define creationTelephone
  (lambda (Numero)
    Numero))

(define creationIdentite
  (lambda (Nom Prenom Naissance ID)
    (cons Nom (cons Prenom (cons Naissance ID)))))

(define creationPersonne
  (lambda (Identite Adresse Tel)
    (cons Identite (cons Adresse Tel))))

(define getNom
  (lambda (P)
    (caar P)))

(define getPrenom
  (lambda (P)
    (cadar P)))

(define getNaissance
  (lambda (P)
    (caddar P)))

(define getID
  (lambda (P)
    (car(cdr(cdr(cdr (car P)))))))

(define getNumeroRue
  (lambda (P)
    (caadr P)))

(define getTelephone
  (lambda (P)
    (caddr P)))

(define getVille
  (lambda (P)
    (car (cdr (cdr (cdr (car (cdr P))))))))


(define present
  (lambda (L n)
    (if (null? L)
        #f
        (if (= n (getID (car L)))
            #t
            (present (cdr L) n)))))

(define getPersonneParIndex
  (lambda (L n)
    (if (= n 1)
        (car L)
        (getPersonneParIndex(cdr L) (- n 1)))))

(define ajoutPersonne
  (lambda (L P)
    (if (present L (getID P))
        "erreur"
        (cons L P))))

(define detruirePersonne
  (lambda (L P)
    (if (null? L)
            '()
            (if (present L (getID P))
                (if (= (getID (car L)) (getID P))
                    (detruirePersonne (cdr L) P)
                    (cons (car L) (detruirePersonne (cdr L) P)))
                "erreur"))))

(define getPersonne
  (lambda (L T P)
    (listeTriee (getPersonne2 L T P))))


(define getPersonne2
  (lambda (L T P)
    (if (null? L)
        "La base est nulle"
        (if (equal? T "identifiant")
            (getPersonneParIdentifiant L P 1)
            (if (equal? T "nom")
                (getPersonneParNom L P 1)
                (if (equal? T "prenom")
                    (getPersonneParPrenom L P 1)
                    (if (equal? T "telephone")
                        (getPersonneParTelephone L P 1)
                        (if (equal? T "naissance")
                            (getPersonneParNaissance L P 1)
                            (if (equal? T "ville")
                                (getPersonneParVille L P 1)
                                "parametre inconnu")))))))))
            
(define getPersonneParIdentifiant
  (lambda (L P n)
    (if (<= n (length L))
        (if (= P (getID (getPersonneParIndex L n)))
            (cons (getPersonneParIndex L n) (getPersonneParIdentifiant L P (+ n 1)))
            (getPersonneParIdentifiant L P (+ n 1)))
        '())))

(define getPersonneParNom
  (lambda (L P n)
    (if (<= n (length L))
        (if (equal? P (getNom (getPersonneParIndex L n)))
            (cons (getPersonneParIndex L n) (getPersonneParNom L P (+ n 1)))
            (getPersonneParNom L P (+ n 1)))
        '())))
        

(define getPersonneParPrenom
  (lambda (L P n)
    (if (<= n (length L))
        (if (equal? P (getPrenom (getPersonneParIndex L n)))
            (cons (getPersonneParIndex L n) (getPersonneParPrenom L P (+ n 1)))
            (getPersonneParPrenom L P (+ n 1)))
        '())))

(define getPersonneParTelephone
  (lambda (L P n)
    (if (<= n (length L))
        (if (equal? P (getTelephone (getPersonneParIndex L n)))
            (cons (getPersonneParIndex L n) (getPersonneParTelephone L P (+ n 1)))
            (getPersonneParTelephone L P (+ n 1)))
        '())))

(define getPersonneParNaissance
  (lambda (L P n)
    (if (<= n (length L))
        (if (equal? P (getNaissance (getPersonneParIndex L n)))
            (cons (getPersonneParIndex L n) (getPersonneParNaissance L P (+ n 1)))
            (getPersonneParNaissance L P (+ n 1)))
        '())))

(define getPersonneParVille
  (lambda (L P n)
    (if (<= n (length L))
        (if (equal? P (getVille (getPersonneParIndex L n)))
            (cons (getPersonneParIndex L n) (getPersonneParVille L P (+ n 1)))
            (getPersonneParVille L P (+ n 1)))
        '())))

(define getAllID
  (lambda (L)
    (sort (getAllID2 L 1) <)))

(define getAllID2
  (lambda (L i)
    (if (> i (length L))
        '()
        (cons (getID (getPersonneParIndex L i)) (getAllID2 L (+ 1 i))))))

(define reconstruireLaListe
  (lambda (L ID)
    (reconstruireLaListe2 L ID 0)))

(define reconstruireLaListe2
  (lambda (L ID i)
    (if (> i (length ID))
        '()
        (cons (getPersonne2 L "identifiant" (car ID)) (reconstruireLaListe2 L (cdr ID) (+ i 1))))))

(define listeTriee
  (lambda (A)
    (reconstruireLaListe L (getAllID A))))
