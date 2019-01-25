(defun !checkIfEnd (rowIndex columnIndex isX state)
    (progn 
        (setq isEnd (checkIfBridgeOrFork (list rowIndex columnIndex (if isX 'x 'o)) state))
        (if (not isEnd)
            (setq isEnd (checkIfRing (list rowIndex columnIndex (if isX 'x 'o)) state))
        )
        isEnd
    )
)
(defun generatePossibleStates (state)
    (cond
        ((null state) '())
        (t (append (generateRowStates (caar state) (cadar state) state) (generatePossibleStates(cdr state))) )
    )
)

(defun generateRowStates (rowIndex row matrix)
    (cond
        ((null row) '())
        ((checkIfValid rowIndex (caar row) matrix) 
            (cons (setField rowIndex (caar row) matrix isX) (generateRowStates rowIndex (cdr row) matrix)))
        (t (generateRowStates rowIndex (cdr row) matrix))
    )
)

(defun areAdjacent (node1 node2 matrix)
    (checkExistance node1 (getNeighbours node2 '() matrix))
)

(defun traverseBridgeAndFork (waiting visited matrix)
    (cond
        ((null waiting) '())
        ((checkBridge (car waiting)) t)
        ((checkFork (car waiting)) t)
        (t 
            (let* 
                (
                    (newVisited (append visited (list (car waiting))))
                    (children (getNeighbours (car waiting) (append visited (cdr waiting)) matrix))
                    (newWaiting (append children (cdr waiting)))
                    (result (traverseBridgeAndFork newWaiting newVisited matrix)) 
                )
                result
            )
        )
    )
)

(defun checkIfBridgeOrFork (lastMove matrix)
    (setq sides (getSides))
    (setq visitedCorners '0)
    (traverseBridgeAndFork (list lastMove) '() matrix)
)

(defun checkBridge (move)
    (if (checkExistance (list (car move) (cadr move)) corners) (setq visitedCorners (1+ visitedCorners)))
    (if (equalp visitedCorners 2) t '())
)

(defun checkFork (move)
    (setq sides (removeFromSides (list (car move) (cadr move)) sides))
    (if (equalp (length sides) 3) t '())
)


(defun getNeighbours (parent visited matrix)
    (let* 
        (
            (rowIndex (car parent))
            (columnIndex (cadr parent))
            (value (caddr parent)) 
            (acc '())
            (acc (if (checkChild (1+ rowIndex) (1+ columnIndex) value visited matrix) (cons (list (1+ rowIndex) (1+ columnIndex) value) acc) acc))
            (acc (if (checkChild (1+ rowIndex) columnIndex value visited matrix) (cons (list (1+ rowIndex) columnIndex value) acc) acc))
            (acc (if (checkChild rowIndex (1+ columnIndex) value visited matrix) (cons (list rowIndex (1+ columnIndex) value) acc) acc))
            (acc (if (checkChild rowIndex (1- columnIndex) value visited matrix) (cons (list rowIndex (1- columnIndex) value) acc) acc))
            (acc (if (checkChild (1- rowIndex) columnIndex value visited matrix) (cons (list (1- rowIndex) columnIndex value) acc) acc))
            (acc (if (checkChild (1- rowIndex) (1- columnIndex) value visited matrix) (cons (list (1- rowIndex) (1- columnIndex) value) acc) acc))
        )       
        acc
    )
)

(defun checkChild (rowIndex columnIndex value visited matrix)
    (and
        (equalp (getValue rowIndex columnIndex matrix) value)
        (not (checkExistance (list rowIndex columnIndex value) visited))
    )
)

(defun checkExistance (element list)
    (cond
        ((null list) '())
        ((equalp element (car list)) t)
        (t (checkExistance element (cdr list)))
    )
)
(defun charToInt (char)
    (- (char-int char) 65)
)

(defun intToChar (int)
    (int-char (+ int 65))
)

(defun getSides ()
    (setq side1 '())
    (setq side2 '())
    (setq side3 '())
    (setq side4 '())
    (setq side5 '())
    (setq side6 '())
    (loop for i from 1 to (- n 2) do
        (setq side1 (cons (list 0 i) side1))
        (setq side2 (cons (list i (+ i (1- n))) side2))
        (setq side3 (cons (list (+ i (1- n)) (* 2 (1- n))) side3))
        (setq side4 (cons (list (* 2 (1- n)) (+ i (1- n))) side4))
        (setq side5 (cons (list (+ i (1- n)) i) side5))        
        (setq side6 (cons (list i 0) side6))    
    )
    (list side1 side2 side3 side4 side5 side6)
)

(defun removeFromSides (element sides)
    (cond
        ((null sides) '())
        ((checkExistance element (car sides)) (removeFromSides element (cdr sides)))
        (t (cons (car sides) (removeFromSides element (cdr sides))))
    )
)

(defun removeFromList (element list)
    (cond
        ((null list) '())
        ((equalp element (car list)) (cdr list))
        (t (cons (car list) (removeFromList element (cdr list))))
    )
)

(defun checkIfRing (lastMove matrix)
    (setq tmpMatrix matrix)
    (callRingTraversal lastMove tmpMatrix)
)
(defun callRingTraversal (lastMove matrix)
    (cond
        ((traverseRing (list lastMove) '() matrix) 
            (if (checkIfValid (car lastMove) (cadr lastMove) matrix)
                '()
                (callRingTraversal lastMove tmpMatrix)
            )
        )
        ((> (countRemaining (list lastMove) '() matrix) 5) t)
        (t '())
    )
)
(defun countRemaining (waiting visited matrix)
    (cond
        ((null waiting) (length visited))
        (t
            (let*
                (
                    (newVisited (cons (car waiting) visited))
                    (unvisitedNeighbours (getNeighbours (car waiting) (append (cdr waiting) visited) matrix))
                    (newWaiting (append unvisitedNeighbours (cdr waiting)))
                )
            
                (countRemaining newWaiting newVisited matrix)
            )
        )
    )
)
(defun traverseRing (waiting visited matrix)
    (cond
        ((null waiting) '())
        (t
            (let*
                (
                    (newVisited (cons (car waiting) visited))
                    (unvisitedNeighbours (getNeighbours (car waiting) (append (cdr waiting) visited) matrix))
                    (newWaiting (append unvisitedNeighbours (cdr waiting)))
                    (allNeighbours (getNeighbours (car waiting) '() matrix))
                )
                (cond
                    (
                        (or
                            (< (length allNeighbours) 2)
                            (and (equalp (length allNeighbours) 2) (areAdjacent (car allNeighbours) (cadr allNeighbours) matrix))
                        )
                        (progn
                            (setq tmpMatrix (resetField (caar waiting) (cadar waiting) matrix))
                            t
                        )
                    )
                    (t (traverseRing newWaiting newVisited matrix))
                )
            )
        )
    )
)