; initialize board for othello
(defun createBoard ()
	(let ((board (make-list 64 :initial-element '-)))
		(setf (nth 27 board) 'W)
		(setf (nth 36 board) 'W)
		(setf (nth 28 board) 'B)
		(setf (nth 35 board) 'B)
		board
	)
)

; print board state
(defun printBoard (board)
	(format t "  1 2 3 4 5 6 7 8~%")
	(dotimes (i 64)
		(cond
			((eq 7 (mod i 8))
				(format t "~s~%" (nth i board)))
			((eq 0 (mod i 8))
				(format t "~d ~s " (1+ (floor (/ i 8))) (nth i board)))
			(t (format t "~s " (nth i board)))
		)
	)
)

; generate successors for piece on board 
(defun gen_moves (pos board))


(defun test_move (plyr pos board)
	(let
		(newboard board)
		(checkpos)
		(oplyr 'W)
	)
	(cond ((eq plyr 'W) (setf oplyr 'B)))
	(dotimes (row 3)
		(dotimes (col 3)
			(setf checkpos (+ (+ (- col 1) pos) (* (- row 1) pos)))
			(cond
				((and (and (< checkpos 64) (> checkpos -1))
					(and (> (+ (mod pos 8) (- col 1)) 0) (< (+ (mod pos 8) (- col 1)) 8))
					(and (eq (nth checkpos board) oplyr)))
					(setf newboard (flip_pieces newboard plyr checkpos (- row 1) (- col 1)))
				)
			)
		)		
	)
	newboard
)

; Still working on this one
(defun flip_pieces (board player position y_vec x_vec)
	(do*
		(
			(alterboard board)
			((nth position alterboard) player)
			(pos (+ (* y_vec 8) x_vec position))
			(princ pos)
			(oplyr 'W)
			;(cond ((eq player 'W) (setf oplyr 'B)))
		)
		((eq (nth pos board) player) (return alterboard))
		
		(setf (nth pos alterboard) player)

		(cond ((or (< (+ pos (* y_vec 8)) 0) (> (+ pos (* y_vec 8)) 64) 
			(< (+ (mod pos 8) x_vec) 0) (> (+ (mod pos 8) x_vec) 7))
			(return board))

			(t (setf pos (+ (* y_vec 8) x_vec pos)))
		)

		(cond ((eq (nth pos board) '-) (return board)))
	) 
)