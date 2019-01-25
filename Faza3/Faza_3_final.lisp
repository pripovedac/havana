(defun generatePossibleMoves (state)
    (cond
        ((null state) '())
        (t (append (generateRowMoves (caar state) (cadar state) state) (generatePossibleMoves (cdr state))) )
    )
)

(defun generateRowMoves (rowIndex row matrix)
    (cond
        ((null row) '())
        ((checkIfValid rowIndex (caar row) matrix) 
            (cons (list rowIndex (caar row)) (generateRowMoves rowIndex (cdr row) matrix)))
        (t (generateRowMoves rowIndex (cdr row) matrix))
    )
)

(defun minimax (state move alpha beta currentDepth isMyMove)
    (cond
        ((and (not (equalp depth currentDepth)) (not (null move)) (!checkIfEnd (car move) (cadr move) (xor isMyMove isX) state))
            (if (not isMyMove) 
                (list move '1000)
                (list move '-1000)
            )
        )
        ((zerop currentDepth) (list move (heuristics state)))
        (t
            (let*
                (
                    (newMovesList (generatePossibleMoves state))
                    (result
                        (if isMyMove
                            (maxPlay newMovesList '() currentDepth alpha beta isMyMove state )
                            (minPlay newMovesList '() currentDepth alpha beta isMyMove state )
                        ) 
                    )
                )
                (cond
                    ((null newMovesList) (list move (heuristics state)))
                    ((equalp currentDepth depth) (car result))
                    (t (list move (cadr result)))
                )
            )
        )       
    )
)

(defun maxPlay (movesList bestMove depth alpha beta isMyMove previousState)
    (cond 
        ((null movesList) (list bestMove alpha))
        (t 
            (let*
                (                
                    (previousMoveState (setField (caar movesList) (cadar movesList) previousState  (not (xor isX isMyMove)) ))
                    (minMove (minimax previousMoveState (car movesList)  alpha beta (1- depth) (not isMyMove)))
                    (newMove (if (>= alpha (cadr minMove)) (list bestMove alpha) minMove))
                )
                (if (or (> (cadr newMove) beta) (null movesList))
                        (list bestMove (cadr newMove))
                        (maxPlay (cdr movesList) (car newMove) depth (cadr newMove) beta isMyMove previousState)
                )
            )
        )
    )
)

(defun minPlay (movesList bestMove depth alpha beta isMyMove previousState)
    (cond 
        ((null movesList) (list bestMove beta))
        (t 
            (let*
                (    
                    (previousMoveState (setField (caar movesList) (cadar movesList) previousState (not (xor isX isMyMove))))            
                    (maxMove (minimax previousMoveState (car movesList) alpha beta (1- depth) (not isMyMove)))
                    (newMove (if (<= beta (cadr maxMove)) (list bestMove beta) maxMove))
                )
                (if (or (< (cadr newMove) alpha)(null (cdr movesList)))
                        (list bestMove (cadr newMove))
                        (minPlay (cdr movesList) (car newMove) depth alpha (cadr newMove) isMyMove previousState)
                )
            )
        )
    )
)