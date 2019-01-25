(defun printMatrix (n matrix)
    (printTopColumnIndex n)
    (loop for i from 0 to (- (* 2 n) 2) do
         (printRow i n (getVisibleFieldsNumber n i) matrix)
    )
)

(defun printTopColumnIndex (n)
    (printEmptySpaces n -2)
    (loop for i from 0 to (1- n) do
        (format t "~a " i)
    )
    (format t "~C" #\linefeed)
)

;; Stampa ceo red: oznaku reda na pocetku, razmake i odgovarajuce karaktere.
(defun printRow (rowIndex n visibleFieldsNumber matrix)
    (printRowIndex rowIndex)
    (printEmptySpaces n rowIndex)
    (printVisibleFields rowIndex visibleFieldsNumber matrix)
    (if (< rowIndex (1- n))(printSideColumnIndex visibleFieldsNumber))
    (format t "~C" #\linefeed)
)

(defun printRowIndex (rowIndex)
    (format t "~a" (int-char (+ rowIndex 65)) )
)

(defun printEmptySpaces (n rowIndex)
    (let ((emptyFieldsNumber (getEmptyFieldsNumber n rowIndex)) )
        (loop for i from 0 to emptyFieldsNumber do
            (format t "~C" #\Space)
        )
    )
)

(defun getEmptyFieldsNumber (n rowIndex)
    (abs(- (- n 1) rowIndex))
)

(defun getVisibleFieldsNumber (n rowIndex)
    (- (- (* 2 n) 1) (getEmptyFieldsNumber n rowIndex) )
)

(defun printSideColumnIndex (visibleFieldsNumber)
    (format t " ~a" visibleFieldsNumber )
)

;; Stampa vidljiva polja u matrici.
(defun printVisibleFields (rowIndex visibleFieldsNumber matrix)
    (if (< rowIndex n)
            (loop for i from 0 to (1- visibleFieldsNumber) do
            (format t "~a " (getValue rowIndex i matrix) ) )
            ;; else
            (loop for i from (getEmptyFieldsNumber n rowIndex) to (- (* 2 n) 2) do
            (format t "~a " (getValue rowIndex i matrix) ) )
    )
)

;; Vraca vrednost elementa u matrici predstavljenoj pomocu asocijativnih listi.
(defun getValue (row column matrix)
    (cadr (assoc column (cadr (assoc row matrix))))
)
(defun getCorners () 
    (list '(0 0) 
    (list '0 (1- n)) 
    (list (1- n) '0)
    (list (1- n) (* 2 (1- n)))
    (list (* 2 (1- n))(1- n))
    (list (* 2 (1- n))(* 2 (1- n))))
)
(defun matrixFactory (rowIndex)
    (cond
        ((> rowIndex (- (* 2 n) 2)) '())
        (t 
            (cons 
                (cons 
                    rowIndex
                    (list (reverse (rowInit rowIndex (if (> rowIndex (- n 2)) (- (* 2 n) 2) (+ rowIndex (1- n)) ) ) ) )
                ) 
                (matrixFactory (1+ rowIndex))
            )
        )
    )
)
;; Inicijalizuje vrste matrice;
(defun rowInit (rowIndex columnIndex)
    (cond
        ((and (< rowIndex (1- n)) (< columnIndex 0)) '() )
        ((and (< columnIndex (getEmptyFieldsNumber n rowIndex)) (>= rowIndex (1- n) )) '())
        (t (cons (cons columnIndex '(-)) (rowInit rowIndex (1- columnIndex)) ))
    )
)



;; Postavlja vrednost polja u matrici na x, ili o.
(defun setField (rowIndex columnIndex matrix isX)
    (cond
        ((null matrix) '())
        ((equalp rowIndex (caar matrix)) (cons (cons (caar matrix) (list (modifyRow columnIndex (cadar matrix) isX))) (cdr matrix) ))
        (t (cons (car matrix) (setField rowIndex columnIndex (cdr matrix) isX) ) )
    )
)

(defun resetField (rowIndex columnIndex matrix)
    (cond
        ((null matrix) '())
        ((equalp rowIndex (caar matrix)) (cons (cons (caar matrix) (list (resetRow columnIndex (cadar matrix)))) (cdr matrix) ))
        (t (cons (car matrix) (resetField rowIndex columnIndex (cdr matrix)) ) )
    )
)

(defun resetRow (columnIndex row)
    (cond
        ((null row) '())
        ((equalp columnIndex (caar row))(cons (cons columnIndex '(-)) (cdr row) ) )
        (t (cons (car row) (resetRow columnIndex (cdr row) ) ) )
    )
)
;; Modifikuje red matrice upisujuci za vrednost polja x, odnosno o.
(defun modifyRow (columnIndex row isX)
    (cond
        ((null row) '())
        ((equalp columnIndex (caar row))(cons (cons columnIndex (list (if isX 'x 'o))) (cdr row) ) )
        (t (cons (car row) (modifyRow columnIndex (cdr row) isX ) ) )
    )
)