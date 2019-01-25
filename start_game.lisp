;; Tim: 7a3
(load "./Faza1/Faza_1_final.lisp")
(load "./Faza2/Faza_2_final.lisp")
(load "./Faza3/Faza_3_final.lisp")
(load "./Faza4/Faza_4_final.lisp")
(load "./Faza4/Inference_engine.cl")

(defun startGame ()
    (readDimension)
    (setq isX t)
    (setq corners (getCorners))
    (setq isEnd '())
    (setq playground (list (matrixFactory '0)))
    (setq isPerson (chooseFirst))
    (setq depth 2)
    (playMove isPerson '())
)
(defun checkIfValid (rowIndex columnIndex matrix)
    (equalp (getValue rowIndex columnIndex matrix) '-)
)
(defun playMove (isPerson previousMove)
    (if (not isEnd)
        (progn
            (if isPerson
                (progn
                    (format t "Enter row and column indexes as a list:~%")
                    (setq entry (read))
                    (setq entry (cons (charToInt (character (car entry))) (cdr entry)))
                    (setq rowIndex (car entry))
                    (setq columnIndex (cadr entry))
                    (setq previousMove (list rowIndex columnIndex))
                    (if (checkIfValid rowIndex columnIndex (car playground))
                        (progn 
                            (setq playground (cons (setField rowIndex columnIndex (car playground) isX) playground))
                            (!checkIfEnd rowIndex columnIndex isX (car playground))
                            
                        )
                        (progn
                            (format t "~%You are invalid. Play again.~%~%")
                            (playMove isPerson '())
                        )
                    )
                )
                (progn
                    (let
                        (
                            (nextMove (minimax (car playground) previousMove -1500 1500 depth t))
                        )
                        (setq playground (cons (setField (car nextMove) (cadr nextMove) (car playground) isX) playground))
                        (!checkIfEnd (car nextMove) (cadr nextMove) isX (car playground))            
                    )  
                )
            )
            (printMatrix n (car playground))
            (setq isX (not isX))  
            (playMove (not isPerson) previousMove)
            ;;(playMove isPerson previousMove)
        )
        (progn           
            (format t "Izigraste se!")
        )
    )
)

(defun readDimension ()
	 (format t "Enter table length:~%")
		(setq n (read))
)


(defun chooseFirst ()
	(format t "Who is playing first? Enter 'c' for computer, or 'p' for person:~%")
	(setq entry (read))
    (if (not (or (equalp entry 'C) (equalp entry 'P) )) 
        (progn (format t "You are invalid.~%") '())
        (progn      
            (cond
                ((equalp entry 'C) '())
                ((equalp entry 'P) t)
            )   
        )
    )
)

(startGame)