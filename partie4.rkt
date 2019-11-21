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
            (getPersonneParIndex L n)
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