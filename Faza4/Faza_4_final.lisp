;;Cinjenice su sva popunjena polja na tabli u formatu (igrac indeksVrste indeksKolone). pr. (X 1 2)
(defun generateFacts (matrix)
	(cond 
		((null matrix) '())
		(t (append (generateFactsRow (caar matrix) (cadar matrix)) (generateFacts (cdr matrix))))
    )
)

(defun generateFactsRow (rowIndex row)
	(cond 
		((null row) '())
        ((not (equalp (cadar row) '-)) (cons (list (cadar row) rowIndex (caar row)) (generateFactsRow rowIndex (cdr row))))
		(t (generateFactsRow rowIndex (cdr row)))
	)
)

(defun =getHeuristicsState ()
    heuristicsState
)

(defun !areNeighbours (rowIndex1 columnIndex1 value1 rowIndex2 columnIndex2 value2)
    (let*
        (
            (neighbours1 (getAllNeighbours (list rowIndex1 columnIndex1 value1) heuristicsState))
        )
        (checkExistance (list rowIndex2 columnIndex2 value2) neighbours1)
    )
)
(defun getAllNeighbours (parent matrix)
    (let* 
        (
            (rowIndex (car parent))
            (columnIndex (cadr parent))
            (value (caddr parent)) 
            (acc1 (getIfSet (1+ rowIndex) (1+ columnIndex) matrix)) 
            (acc2 (getIfSet (1+ rowIndex) columnIndex matrix))
            (acc3 (getIfSet rowIndex (1+ columnIndex) matrix))
            (acc4 (getIfSet rowIndex (1- columnIndex) matrix))
            (acc5 (getIfSet (1- rowIndex) columnIndex matrix))
            (acc6 (getIfSet (1- rowIndex) (1- columnIndex) matrix))
        )       
        (append acc1 acc2 acc3 acc4 acc5 acc6)
    )
)

(defun getIfSet (rowIndex columnIndex matrix)
    (let*
        (
            (valueAt (getValue rowIndex columnIndex matrix))
        )
        (if (or (equalp valueAt 'x) (equalp valueAt 'o))  (list (list rowIndex columnIndex valueAt)) '())
    )
)

(defun !isCorner (rowIndex columnIndex)
    (checkExistance (list rowIndex columnIndex) corners)
)

(defun !negation (x)
    (not x)
)
(defparameter *T1-RULES* '(
	(if (and (X ?a ?b) (!checkIfEnd ?a ?b t (=getHeuristicsState))) then (endX))
	(if (and (O ?a ?b) (!checkIfEnd ?a ?b () (=getHeuristicsState))) then (endO))
    (if (and (X ?a ?b) (O ?c ?d) (!areNeighbours ?a ?b 'x ?c ?d 'o) (!isCorner ?c ?d)) then (blockedCornerX))
    (if (and (O ?a ?b) (X ?c ?d) (!areNeighbours ?a ?b 'o ?c ?d 'x) (!isCorner ?c ?d)) then (blockedCornerO))
   ;; (if (and (X ?a ?b) (O ?c ?d) (X ?e ?f) (!areNeighbours ?a ?b 'x ?c ?d 'o) (!areNeighbours ?c ?d 'o ?e ?f 'x) (!negation (!areNeighbours ?a ?b 'x ?e ?f 'x))) then (blockedConnectionX))
   ;; (if (and (O ?a ?b) (X ?c ?d) (O ?e ?f) (!areNeighbours ?a ?b 'o ?c ?d 'x) (!areNeighbours ?c ?d 'x ?e ?f 'o) (!negation (!areNeighbours ?a ?b 'o ?e ?f 'o))) then (blockedConnectionO))
))

(defun heuristics (state)
    (let* 
        (
            (*T1-FACTS* (generateFacts state))
        )
        (progn
            (setq heuristicsState state)
            (prepare-knowledge *T1-RULES* *T1-FACTS* 10)
            (let*
                (
                    (acc1 (if (/= (count-results '(endX)) 0) (if (not isPerson) '1000 '-1000) '0))
                    (acc2 (if (/= (count-results '(endO)) 0) (if (not isPerson) '-1000 '1000) '0))
                    (acc3 (* (count-results '(blockedCornerO)) (if (not isPerson) '50 '-50)))
                    (acc4 (* (count-results '(blockedCornerX)) (if (not isPerson) '-50 '50)))
                   ;; (acc5 (* (count-results '(blockedConnectionO)) (if (not isPerson) '20 '-20)))
                    ;;(acc6 (* (count-results '(blockedConnectionX)) (if (not isPerson) '-20 '20)))
                    ;;(acc (+ acc1 acc2 acc3 acc4 acc5 acc6))
                    (acc (+ acc1 acc2 acc3 acc4))
                )
                acc
            )
        )
    )
)