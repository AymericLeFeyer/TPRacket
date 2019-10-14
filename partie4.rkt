#lang racket

(define L' ((("Helin" "Dylan" "29/10/1999" 1) (60 "rue Waldeck Rousseaux" 59410 "Anzin" "France") "0652274310")
            (("Helin" "Steven" "29/10/1999" 2) (60 "rue Waldeck Rousseaux" 59410 "Anzin" "France") "0652274310")
            (("Baudelet" "Conrad" "29/10/1999" 3) (60 "rue Waldeck Rousseaux" 59410 "Anzin" "France") "0652274310")))

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

        
        
  
               
  