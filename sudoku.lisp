;;; VARIABLES GLOBALES ;;;

; Drapeau pour valider si la grille est complete 
(defparameter *grille-est-complete* 0)
; Drapeau pour valider si les regles ont été respecté
(defparameter *regles-respectees* 0)
; 0 est là car delete ne va pas supprimer le premier élément dans la liste
; À remplir lors de l'exécution 
(defparameter *valeurs-possibles* '())
; Modèle pour vérifier que l'utilisateur n'écrase pas les chiffres donnés au début du jeu.  
(defparameter *liste-grilles* '()) 
; Taille de la grille
(defparameter *taille-grille* 0) 
; Liste de la première case de chaque carré
(defparameter *liste-premiere-case* '())

;;; MÉTHODES GÉNÉRALES ;;; 


;; Afficher la grille en fonction de sa taille 
(defun afficher-grille (grille)
  (setq *rang* 1)
  (defparameter *rang-lettre* 0)
  (setq *tiret* (string " *"))
  (defparameter *alphabet* (list 'A 'B 'C 'D 'E 'F 'G 'H 'I 'J 'K 'M 'L 'N 'O 'P 'Q 'R 'S 'T 'U 'V 'W 'X 'Y 'Z))
  (format t "    ")
  (loop for a from 0 to (1- *taille-grille*) do
		(format t "~A   " (nth *rang-lettre* *alphabet*))
        (setq *rang-lettre* (1+ *rang-lettre*))
        (setq *tiret* (concatenate 'string *tiret* "****")))
   (format t "~% ~A ~%" *tiret*)
   (setq *var_colonne* (nth (1- *rang-lettre*) *alphabet*))
  (loop for i below (car (array-dimensions grille)) do
       (format t "~a " *rang*)
       (setq *rang* (1+ *rang*))
       (loop for j below (cadr (array-dimensions grille)) do
		  (progn
		    (if (zerop (aref grille i j))
		      (setq *cellule* " ")
		      (setq *cellule* (aref grille i j)))                    
		    (if (zerop (mod j (isqrt *taille-grille*)))
			  (format t "| ~a " *cellule*)
			  (format t "  ~a " *cellule*))))
       (format t "|")
       (if (zerop (mod (1- *rang*) (isqrt *taille-grille*)))
	     (format t "~% ~A ~%" *tiret*)
	     (format t "~%"))))
	   

;; Demander un chiffre à l'utilisateur
(defun demander-chiffre ()
   (defparameter *drapeau-chiffre* 0)  
   (loop do
      (format t "Choisissez un chiffre entre 1 et ~D: " *taille-grille*)
      (defparameter *chiffre* (parse-integer(read-line) ))	
	  (if (and (> *chiffre* 0) (< *chiffre* (1+ *taille-grille*)))
		(progn (setf *drapeau-chiffre* 0) *chiffre*)
		(progn (format t "Ce chiffre n'est pas valable. ") (setf *drapeau-chiffre* 1)))
     until (zerop *drapeau-chiffre*))
    (clear-input))
         
;; Demander une colonne à l'utilisateur
(defun demander-colonne ()
   (defparameter *drapeau-col* 0)
   (loop do
      (format t "Choisissez une colonne (A-~A): " *var_colonne*)
      (defparameter *colonne* (char-code (char (read-line) 0)))
	 (cond ((and (< *colonne* (+ 65 *taille-grille*)) (> *colonne* 64)) (progn (setf *drapeau-col* 0) (setf *colonne* (- *colonne* 65))))  ; pour les majuscules (ASCII)
               ((and (< *colonne* (+ 97 *taille-grille*)) (> *colonne* 96)) (progn (setf *drapeau-col* 0) (setf *colonne* (- *colonne* 97))))  ;pour les minuscules
	       (t (progn (format t "Cette colonne n'est pas valable. ") (setf *drapeau-col* 1))))
    until (zerop *drapeau-col*))
    (clear-input))
         
;; Demander un rang à l'utilisateur
(defun demander-rang ()
   (defparameter *drapeau-rang* 0)  
   (loop do
      (format t "Choisissez un rang (1-~D): " *taille-grille*)
      (defparameter *rang* (parse-integer(read-line) ))	
	  (if (and (> *rang* 0) (< *rang* (1+ *taille-grille*)))
		(progn (setf *drapeau-rang* 0) (setf *rang* (1- *rang*)))
		(progn (format t "Ce chiffre n'est pas valable. ") (setf *drapeau-rang* 1)))
     until (zerop *drapeau-rang*))
    (clear-input))

;; Remplir une case dans la grille
(defun remplir-grille (y x grille chiffre)
   (defparameter *drapeau-placement* 0)
      (verifier-case-modifiable x y *grille-modele*)
      (if (zerop *drapeau-placement*)
         (setf (aref grille x y) chiffre) 
        (format t "Vous ne pouvez modifier cette case. ~%"))) 



;; Determiner la taille de la grille
(defun determiner-taille-grille (grille)
  (setq *taille-grille* (list-length (first grille))))      

;; Determiner toutes les valeurs possibles dans la grille
(defun determiner-valeurs-possibles ()
  (loop for y from 0 to *taille-grille* do 
   (setq *valeurs-possibles* (append *valeurs-possibles* (list y)))))


;;; MÉTHODES DE VÉRIFICATION/VALIDATION ;;;

;; Vérifer que la grille respecte toutes les regles du jeu (sans considérer les 0)
(defun valider-grille (grille)
  (valider-rangs-et-colonnes grille)
  (valider-carre grille))

;; Vérifier que aucune case n'est pas vide
(defun verifier-toute-case-complete (grille)
   (defparameter *grille-est-complete* 0)
   (loop for x from 0 to (1- *taille-grille*) do
     (loop for y from 0 to (1- *taille-grille*) do
       (if (zerop (aref grille x y))
        (setq *grille-est-complete* 1)))))

;; Vérifier si case est modifiable
(defun verifier-case-modifiable (x y modele) 
  (if (zerop (aref modele x y))
     (setf *drapeau-placement* 0)    ; Modifiable 
     (setf *drapeau-placement* 1)))  ; Non-modifiable

;; Fonction pour vérifier si ,une fois la grille remplie , elle est gagnante ou pas
(defun verifier-solution(grille solution)
   (defparameter *drapeau* 0)
   (loop for y from 0 to (1- *taille-grille*) do
     (if (/= *drapeau* 1)
      (loop for x from 0 to (1- *taille-grille*) do
         (if (/= *drapeau* 1)
            (if (/= (aref grille x y) (aref solution x y))
               (setf *drapeau* 1))))))
      (if (/= *drapeau* 1)
        (format t "Félicitations ! Vous avez gagné !~%")
        (format t "Vous n'avez pas la bonne solution! Trouvez votre erreur pour gagner !~%")))



;;; MÉTHODES AUXILIAIRES ;;;

;; Créer une liste composée des cordonnées de la case la plus en haut à gauche pour chaque carré
(defun creer-liste-carre ()
  (loop for y from 0 to (1- (isqrt *taille-grille*)) do
    (loop for x from 0 to (1- (isqrt *taille-grille*)) do  
      (setq *liste-premiere-case* (append *liste-premiere-case* (list (list (* (isqrt *taille-grille*) x) (* (isqrt *taille-grille*) y))))))))


;; Vérifier que chaque chiffre apparait uniquement une fois dans chaque carré
(defun valider-carre(grille)
  (loop for x in *liste-premiere-case* do 
    (if (= 0 *regles-respectees*)
    (progn (parcourir-carre grille (first x) (second x)))))) 

;; Valider qu'il y a aucune repétition dans le carré (sans considérer les 0)
(defun parcourir-carre (grille x y) 
  (setq *chiffres* '())
  (setq *regles-respectees* 0)
  (loop for a from 0 to (1- (isqrt *taille-grille*)) do
    (loop for b from 0 to (1- (isqrt *taille-grille*)) do 
      (if (not (zerop (aref grille (+ x a) (+ y b))))
        (if (null (find (aref grille (+ x a) (+ y b)) *chiffres*)) ; Si le chiffre n'est pas dans la liste
         (setq *chiffres* (append *chiffres* (list (aref grille (+ x a) (+ y b))))) 
         (setq *regles-respectees* 1))))))


;; Convertir liste en tableau
(defun convertir-liste-tableau (liste)
	    (make-array (list (length liste)
			      (length (first liste)))
			:initial-contents liste))

;; Convertir tableau en liste
(defun convertir-tableau-liste (tableau)
	   (loop for i below (array-dimension tableau 0)
		 collect (loop for j below (array-dimension tableau 1)
			       collect (aref tableau i j))))

;; Valider qu'il y a aucune répétition dans les colonnes et les rangs (sans considérer les 0)
(defun valider-rangs-et-colonnes (grille)
  (defparameter *regles-respectees* 0)
  (setq *liste-chiffres* '())
  (setq *liste-chiffres-2* '())
  (loop for x from 0 to (1- *taille-grille*) do
    (loop for y from 0 to (1- *taille-grille*) do
      (if (and (not (zerop (aref grille x y))) (not (zerop (aref grille y x))))
        (progn (format t " " (aref grille x y)) 
        (if (or (find (aref grille x y) *liste-chiffres*) (find (aref grille y x) *liste-chiffres-2*))
           (progn (setf *regles-respectees* 1)(return))
           (progn (setf *liste-chiffres* (append *liste-chiffres* (list (aref grille x y)))) (setf *liste-chiffres-2* (append *liste-chiffres-2* (list (aref grille y x)))))))))
   (if (= *regles-respectees* 1) 
     (return)
     (progn (setf *liste-chiffres-2* '())(setf *liste-chiffres* '()))))
    (if (/= *regles-respectees* 1) grille)) 


;;; Méthodes aléatoires ;;;

;; Générer les variables aléatoirement
(defun generer-variables-aleatoires (modele)
  (setf *drap-aleat* 0)
  (loop do 
    (defparameter *colonne* (random *taille-grille*)) 
    (defparameter *rang* (random *taille-grille*))
    (if (zerop (aref modele *rang* *colonne*))
      (setf *drap-aleat* 1)) 
  until (= *drap-aleat* 1)) 
  (defparameter *chiffre* (1+ (random *taille-grille*)))); Chiffre entre 1 et *taille-grille*
  



;;; Methodes intelligence-artificielle ;;;

;; Choisir un chiffre
(defun IA-choisir-chiffre (grille x y valeurs)
  (loop for (a b) in *liste-premiere-case* do 
   (if (and (and (>= x a) (< x (+ a (isqrt *taille-grille*)))) (and (>= y b) (< y (+ b (isqrt *taille-grille*))))) 
     (IA-parcourir-carre grille a b valeurs))) valeurs)

;; Parcourir le carré
(defun IA-parcourir-carre (grille x y valeurs) 
  (loop for a from x to (+ (1- (isqrt *taille-grille*)) x) do
    (loop for b from y to (+ (1- (isqrt *taille-grille*)) y) do 
       (delete (aref grille a b) valeurs)))
  valeurs)

;; Enlever les valeurs trouvées dans la colonne
(defun IA-parcourir-colonne (grille x valeurs)
  (loop for y from 0 to (1- *taille-grille*) do
    (delete (aref grille x y) valeurs)))

;; Enlever les valeurs trouvées dans le rang
(defun IA-parcourir-rang (grille y valeurs)
  (loop for x from 0 to (1- *taille-grille*) do 
    (delete (aref grille x y) valeurs)))

;; Retourner une liste de valeurs possibles pour une case
;; Si la taille de la liste = 2 (c'est à dire 0 et un autre chiffre sont les seuls chiffres présent dans la liste)
;; on va mettre le chiffre dans la case tout de suite. 
;; NOTE: On a implémenté cette méthode dans une façon assez bizarre à fin de satisfaire votre démande (main-standalone)

(defun IA-determiner-valeurs-possibles(grille x y)
  (setq *temp-vp* (copy-list *valeurs-possibles*))
  (IA-choisir-chiffre grille x y *temp-vp*)
  (IA-parcourir-rang grille y *temp-vp*)
  (IA-parcourir-colonne grille x *temp-vp*)
  (if (= 2 (list-length *temp-vp*))
    (progn (setf (aref grille x y) (second *temp-vp*))(setf *drapeau-solution-complexe* 0)(setq *rang* x)(setq *colonne* y) (setq *chiffre* (second *temp-vp*)))) ; Solution est simple 
  *temp-vp*)  

;; Retourner une liste de forme ((#ValeursPossibles x y)(#ValeursPossibles2 x2 y2)...) 
;; pour chaque case pour déterminer quelle case est à remplir d'abord.
(defun IA-determiner-solutions-possibles (grille)
  (defparameter *liste* '())
  (setq *drapeau-solution-complexe* 1) ; 1 si la solution est complexe, 0 si elle est simple 
  (loop for x from 0 to (1- *taille-grille*) do 
    (loop for y from 0 to (1- *taille-grille*) do
      (if (zerop (aref grille x y))
         (setq *liste* (append *liste* (list (list (1- (length (IA-determiner-valeurs-possibles grille x y)))x y)))))))
   (setq *liste* (reverse (sort *liste* #'> :key #'car)))
   *liste*)

;; Retourner une liste de forme ((x y ... solutionsPossibles)(x2 y2 ... solutionsPossibles2)... grille)
(defun backtrack(liste grille)
  (defparameter *solutions-complexes* '())
  (loop for x in liste do
    (if (and (> (first x) 1) (zerop (list-length *solutions-complexes*)))
      (progn (setq *temp* (IA-determiner-valeurs-possibles grille (second x) (third x))) 
        (setq *solutions-complexes* (append *solutions-complexes* (list (second x) (third x))(cdr *temp*))))))*solutions-complexes*)


;; Donner une liste de solutions possibles, essayer d'aller plus en profondeur avec chaque solution proposée
(defun IA-tester-possibilities (grille liste) 
  (loop for i in (cdr (cdr liste)) do
    (setq *grille-actuelle* (convertir-liste-tableau (first (last *liste-grilles*))))
    (remplir-grille  (second liste) (first liste) *grille-actuelle* i)
    (IA-aller-plus-en-profondeur *grille-actuelle*)))

;; Essayer de remplir la grille.  Si la grille n'est plus valable, revenir en arrière. Si elle est valable, continuer d'aller plus en profondeur 
(defun IA-aller-plus-en-profondeur (grille )
  (setq *liste-grilles* (append *liste-grilles* (list (convertir-tableau-liste grille))))
  (setq *solutions* (IA-determiner-solutions-possibles grille))
  (valider-grille grille)
  (if (zerop *regles-respectees*) 
      (progn
        (verifier-toute-case-complete grille)
        (if (zerop *grille-est-complete*)
          (setq *grille-solution* grille)
          (progn (if (= (first (first *solutions*)) 1)
            (IA-aller-plus-en-profondeur grille))
          (if (>= (first (first *solutions*)) 2)
            (IA-tester-possibilities grille (backtrack *solutions* grille)))))))
  (setq *liste-grilles* (remove (first (last *liste-grilles*)) *liste-grilles* )))


;; Vider les variables
(defun vider-toute-variable()
  (defparameter *grille-est-complete* 0)
  (defparameter *regles-respectees* 0)
  (defparameter *valeurs-possibles* '())
  (defparameter *liste-grilles* '())  
  (defparameter *taille-grille* 0)
  (defparameter *liste-premiere-case* '()))

;; Initialiser quelques variables 
(defun initialiser-variables (grille)
  (vider-toute-variable)
  (defparameter *drapeau-solution-complexe* 1)
  (determiner-taille-grille (convertir-tableau-liste grille))
  (determiner-valeurs-possibles)
  (creer-liste-carre))


;;; METHODES OBLIGATOIRES ;;;

;; Initialiser toutes les variables et déterminer la solution de la grille fournie.
(defun init-standalone (grille)
 (initialiser-variables grille)

(defparameter *grille-modele* (convertir-liste-tableau (copy-list (convertir-tableau-liste grille))))
  (defparameter *grille-modifiable* (convertir-liste-tableau (copy-list (convertir-tableau-liste grille))))
  (defparameter *grille-solution* grille)

  (loop do
    (setq *solutions* (IA-determiner-solutions-possibles *grille-solution*))
    (verifier-toute-case-complete *grille-solution*)
    (if (and (= *drapeau-solution-complexe* 1)(= 1 *grille-est-complete*))
      (progn (setq *liste-grilles* (append *liste-grilles*  (list (convertir-tableau-liste *grille-solution*)))) 
             (IA-tester-possibilities *grille-solution* (backtrack *solutions* *grille-solution*))))
    (verifier-toute-case-complete *grille-solution*)
   until (= *grille-est-complete* 0)))


(defun main-standalone()
  (defparameter *drapeau-modifiable* 0) ; pas modifiable
  (verifier-case-modifiable *rang* *colonne* *grille-modele*)
 (if (zerop *drapeau-modifiable*) 
  (values *rang* *colonne* *chiffre*) 
  (values))) 
  

  
;;; LE JEU ;;;

(defun sudoku(grille)
   (format t "Bienvenue ! Bienvenido ! Wilkommen ! ~%")
   (defparameter *drapeau* 1)
   (loop do 
      (init-standalone (convertir-liste-tableau (copy-list (convertir-tableau-liste grille))))  
      (format t "Choisissez votre mode : ~%")
      (format t "'interactif' pour jouer tout seul, 'aleatoire' pour voir la stratégie aléatoire, ou 'ia' pour résoudre la grille fournie avec une Intelligence Artificielle : ")
      (defparameter *jeu* (read-line))
      (cond 
         ((string-equal *jeu* "interactif") (sudoku-interactif))
         ((string-equal *jeu* "aleatoire") (sudoku-aleatoire))
         ((string-equal *jeu* "ia") (sudoku-ia) )
         (t (format t "Cette option n'existe pas.  Veuillez réessayer. ~%")))
   until (zerop *drapeau*)))

(defun sudoku-interactif()
   (loop do 
      (afficher-grille *grille-modifiable*)
      (demander-rang)
      (demander-colonne)
      (demander-chiffre)
      (remplir-grille *colonne* *rang* *grille-modifiable* *chiffre*)
      (verifier-toute-case-complete *grille-modifiable*) 
      (if (zerop *grille-est-complete*)
        (verifier-solution *grille-modifiable* *grille-solution*))
    until (zerop *drapeau*)))

(defun sudoku-aleatoire()
  (afficher-grille *grille-modifiable*)
  (format t "Veuillez patienter -- la version aléatoire peut prendre du temps à compléter. ~%")
  (loop do
     (generer-variables-aleatoires *grille-modele*)
     (remplir-grille *colonne* *rang* *grille-modifiable* *chiffre*)
     (verifier-toute-case-complete *grille-modifiable*) 
     (if (zerop *grille-est-complete*)
       (verifier-solution *grille-modifiable* *grille-solution*))
  until (zerop *drapeau*))
  (afficher-grille *grille-modifiable*))


(defun sudoku-ia()
  (afficher-grille *grille-solution*)
  (format t "~% Voila ! Notre Intelligence Artificielle a résolu la grille que vous avez fournie. ~%")
  (setq *drapeau* 0))


