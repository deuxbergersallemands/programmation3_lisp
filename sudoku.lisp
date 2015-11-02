(defparameter *solution-liste* (list (list 2 4 5 1 7 9 3 8 6) (list 1 9 6 8 2 3 5 4 7) (list 8 3 7 4 6 5 2 1 9) (list 6 8 4 5 1 7 9 2 3) (list 3 2 9 6 8 4 7 5 1) (list 5 7 1 3 9 2 4 6 8) (list 4 6 2 9 3 8 1 7 5) (list 7 1 3 2 5 6 8 9 4) (list 9 5 8 7 4 1 6 3 2)))

(defparameter *solution-avec-zero* (list (list 0 4 0 0 0 0 3 0 0) (list 0 0 6 8 2 0 0 4 7) (list 0 3 0 4 0 5 0 0 9) (list 0 8 4 0 0 7 9 0 0) (list 3 2 0 0 8 0 0 5 1) (list 0 0 1 3 0 0 4 6 0) (list 4 0 0 9 0 8 0 7 0) (list 7 1 0 0 5 6 8 0 0) (list 0 0 8 0 0 0 0 3 0) ))


; Modèle pour vérifier que l'utilisateur n'écrase pas les chiffres donnés au début du jeu.  
(defparameter *grille-modele* (make-array '(9 9) :initial-contents *solution-avec-zero*))

(defparameter *grille-modifiable* (make-array '(9 9) :initial-contents *solution-avec-zero*))

(defparameter *grille-solution* (make-array '(9 9) :initial-contents *solution-liste*))

;;; FONCTIONS ;;;

(defun afficher-grille (grille)
  (format t "~% ----------------------------------- ~%")
  (loop for i below (car (array-dimensions grille)) do
        (loop for j below (cadr (array-dimensions grille)) do
          (let ((cellule (aref grille i j)))
            (format t "| ~a " cellule)))
        (format t "| ~% ----------------------------------- ~%")))

(defun demander-chiffre ()
   (defparameter *drapeau-chif* 0)    
   (loop do
      (format t "Choissisez un chiffre entre 1 et 9: ")
      (defparameter *chiffre* (parse-integer (read-line)))
      (if (and (> *chiffre* 0) (< *chiffre* 10))
         (progn (setf *drapeau-chif* 0) 
         *chiffre*)
        (progn (format t "Veuillez essayer à nouveau. ") (setf *drapeau-chif* 1)))
     until (= 0 *drapeau-chif*)))


(defun demander-colonne ()
   (defparameter *drapeau-col* 0)
   (loop do
      (format t "Choissisez une colonne (A-I): ")
      (defparameter *colonne* (char-code (read-char)))
      (if (and (< *colonne* 74) (> *colonne* 64))
         (progn (setf *drapeau-col* 0)
             (setf *colonne* (- *colonne* 65)))
         (progn (format t "Veuillez essayer à nouveau. ") (setf *drapeau-col* 1)))
    until (= 0 *drapeau-col*)))
         

(defun demander-rang ()
   (defparameter *drapeau-rang* 0)  
   (loop do
      (format t "Choissisez un rang (1-9): ")
      (defparameter *rang* (parse-integer (read-line)))
      (if (and (> *rang* 0) (< *rang* 10))
         (progn (setf *drapeau-rang* 0) 
         (setf *rang* (- *rang* 1)))
        (progn (format t "Veuillez essayer à nouveau. ") (setf *drapeau-rang* 1)))
     until (= 0 *drapeau-rang*)))


(defun verifier-modele (x y modele) 
  (if (= 0 (aref modele x y))
     (setf *drapeau-placement* 0)    ; Modifiable 
     (setf *drapeau-placement* 1)))  ; Non-modifiable

(defun remplir-grille (x y grille chiffre)
   (defparameter *drapeau-placement* 0)
      (verifier-modele x y *grille-modele*)
      (if (= 0 *drapeau-placement*)
         (setf (aref grille x y) chiffre) 
         (format t "Veuillez essayer à nouveau.  "))) 
      
    


;(defun verifier-solution())
   
;;; LE JEU ;;;

(defun sudoku()
    
)
