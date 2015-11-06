(defparameter *solution-liste* (list (list 2 4 5 1 7 9 3 8 6) (list 1 9 6 8 2 3 5 4 7) (list 8 3 7 4 6 5 2 1 9) (list 6 8 4 5 1 7 9 2 3) (list 3 2 9 6 8 4 7 5 1) (list 5 7 1 3 9 2 4 6 8) (list 4 6 2 9 3 8 1 7 5) (list 7 1 3 2 5 6 8 9 4) (list 9 5 8 7 4 1 6 3 2)))

(defparameter *solution-liste-presque* (list (list 0 4 5 1 7 9 3 8 6) (list 0 9 6 8 2 3 5 4 7) (list 8 3 7 4 6 5 2 1 9) (list 6 8 4 5 1 7 9 2 3) (list 3 2 9 6 8 4 7 5 1) (list 5 7 1 3 9 2 4 6 8) (list 4 6 2 9 3 8 1 7 5) (list 7 1 3 2 5 6 8 9 4) (list 9 5 8 7 4 1 6 3 2)))


(defparameter *solution-avec-zero* (list (list 0 4 0 0 0 0 3 0 0) (list 0 0 6 8 2 0 0 4 7) (list 0 3 0 4 0 5 0 0 9) (list 0 8 4 0 0 7 9 0 0) (list 3 2 0 0 8 0 0 5 1) (list 0 0 1 3 0 0 4 6 0) (list 4 0 0 9 0 8 0 7 0) (list 7 1 0 0 5 6 8 0 0) (list 0 0 8 0 0 0 0 3 0) ))


; ModÃ¨le pour vÃ©rifier que l'utilisateur n'Ã©crase pas les chiffres donnÃ©s au dÃ©but du jeu.  
(defparameter *grille-modele* (make-array '(9 9) :initial-contents *solution-avec-zero*))

(defparameter *grille-modifiable* (make-array '(9 9) :initial-contents *solution-avec-zero*))

(defparameter *grille-vite* (make-array '(9 9) :initial-contents *solution-liste-presque*))


(defparameter *grille-solution* (make-array '(9 9) :initial-contents *solution-liste*))

;;; FONCTIONS ;;;

(defun afficher-grille (grille)
  (format t "~%  A   B   C   D   E   F   G   H   I  ")
  (format t "~% ----------------------------------- ~%")
  (loop for i below (car (array-dimensions grille)) do
        (loop for j below (cadr (array-dimensions grille)) do
          (let ((cellule (aref grille i j)))
            (format t "| ~a " cellule)))
        (format t "| ~% ----------------------------------- ~%")))

(defun demander-chiffre ()
   (defparameter *drapeau-chif* 0)    
   (loop do
      (format t "Choisissez un chiffre entre 1 et 9: ")
      (defparameter *chiffre* (parse-integer (read-line)))
      (if (and (> *chiffre* 0) (< *chiffre* 10))
         (progn (setf *drapeau-chif* 0) 
         *chiffre*)
        (progn (format t "Ce chiffre n'est pas valable. ") (setf *drapeau-chif* 1)))
     until (zerop *drapeau-chif*)))


(defun demander-colonne ()
   (defparameter *drapeau-col* 0)
   (loop do
      (format t "Choisissez une colonne (A-I): ")
      (defparameter *colonne* (char-code (char (read-line) 0)))
	 (cond ((and (< *colonne* 74) (> *colonne* 64)) (progn (setf *drapeau-col* 0) (setf *colonne* (- *colonne* 65))))  ; pour les majuscules
               ((and (< *colonne* 106) (> *colonne* 96)) (progn (setf *drapeau-col* 0) (setf *colonne* (- *colonne* 97))))  ;pour les minuscules
	       (t (progn (format t "Cette colonne n'est pas valable. ") (setf *drapeau-col* 1))))
    until (zerop *drapeau-col*)))
         

(defun demander-rang ()
   (defparameter *drapeau-rang* 0)  
   (loop do
      (format t "Choisissez un rang (1-9): ")
      (defparameter *rang* (parse-integer (read-line)))
      (if (and (> *rang* 0) (< *rang* 10))
         (progn (setf *drapeau-rang* 0) 
         (setf *rang* (- *rang* 1)))
        (progn (format t "Ce rang n'est pas valable. ") (setf *drapeau-rang* 1)))
     until (zerop *drapeau-rang*))
    (clear-input))


(defun verifier-modele (x y modele) 
  (if (= 0 (aref modele x y))
     (setf *drapeau-placement* 0)    ; Modifiable 
     (setf *drapeau-placement* 1)))  ; Non-modifiable

(defun remplir-grille (y x grille chiffre)
   (defparameter *drapeau-placement* 0)
      (verifier-modele x y *grille-modele*)
      (if (= 0 *drapeau-placement*)
         (setf (aref grille x y) chiffre) 
        ; (format t "Vous ne pouvez modifier cette cellule. ~%")  ; DÃ©commentÃ© car version alÃ©atoire prend beaucoup de temps
      )) 
      
      
; fontion appelé à chaque tour pour savoir si la grille est fini ou pas      
(defun verifier-grille-rempli (grille)
   (defparameter *drapeau-rempli* 0)
    (loop for y from 0 to 8 do
     (if (/= *drapeau-rempli* 1)
      (loop for x from 0 to 8 do
         (if (/= *drapeau-rempli* 1)
            (if (zerop (aref grille x y) )
               (setf *drapeau-rempli* 1)))))
	       )
   (if (zerop *drapeau-rempli* )
	(progn(format t "La grille est entièrement remplie !~%") (verifier-solution grille *grille-solution*))
       (format t "La grille n'est pas entièrement remplie !~%"))
  )
  
      
;fonction pour savoir si ,une fois la grille rempli , elle est gagnante ou pas
(defun verifier-solution(grille solution)
   (defparameter *drapeau* 0)
   (loop for y from 0 to 8 do
     (if (/= *drapeau* 1)
      (loop for x from 0 to 8 do
         (if (/= *drapeau* 1)
            (if (/= (aref grille x y) (aref solution x y))
               (setf *drapeau* 1))))))
   (if (/= *drapeau* 1)
      (format t "Félicitations ! Vous avez gagné !~%")
      (format t "Vous n'avez pas la bonne solution! Trouvez votre erreur pour gagné !~%")
   ))


(defun generer-variables-aleatoires ()
  (defparameter *colonne* (random 9))
  (defparameter *rang* (random 9))
  (defparameter *chiffre* (1+ (random 9)))) ; Chiffre entre 1 et 9
  


   
;;; LE JEU ;;;

(defun sudoku()
   (format t "Welcome ! Bienvenue ! Bienvenido ! Wilkommen ! ~%")
   (defparameter *drapeau* 1)
   (loop do 
      (format t "Choisissez votre mode : ~%")
      (format t "'interactif' pour jouer tout seul, 'aleatoire' pour voir la stratégie aléatoire, ou 'ia' pour voir une IA jouer : ")
      (defparameter *jeu* (read-line))
      (cond 
         ((string-equal *jeu* "interactif") (sudoku-interactive))
         ((string-equal *jeu* "aleatoire") (sudoku-aleatoire))
         ((string-equal *jeu* "ia") (format t "Désolé, cette version n'est pas encore disponible. ~%")) 
         (t (format t "Cet option n'existe pas.  Veuillez réessayer. ~%")))
   until (zerop *drapeau*)))

(defun sudoku-interactive()
   (loop do 
      (afficher-grille *grille-vite*)
      (demander-rang)
      (demander-colonne)
      (demander-chiffre)
      (remplir-grille *colonne* *rang* *grille-vite* *chiffre*)
      (verifier-grille-rempli *grille-vite*)
    until (zerop *drapeau*)))

(defun sudoku-aleatoire()
  (afficher-grille *grille-vite*)
  (format t "Veuillez patienter -- la version aléatoire peut prendre du temps à compléter.")
  (loop do
     (generer-variables-aleatoires)
     (remplir-grille *colonne* *rang* *grille-vite* *chiffre*)
     (verifier-grille-rempli *grille-vite*)
  until (zerop *drapeau*))
  (afficher-grille *grille-visite*))
