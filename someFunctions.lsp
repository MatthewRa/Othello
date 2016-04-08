(defstruct node
	state ; 8x8 representation of board
	player ; current player
	black ; number of black pieces on board
	white ; number of white pieces on board
	score ; score assigned by evaluation of state or pieces
)

(defun StartGame(playerColor)

	(setf board (createBoard))
	(printBoard board)
	
	(cond
		((equalp playerColor 'B)
			; Player Makes Move
			(format t "~%What is your move [row col]? ")
	
			(setf move (list (read) (read)))
			
			(format t "Your move: ~A" move)
			
			; Computer Makes Move
		)
		
		((equalp playerColor 'W)
			; Computer Makes Move
			
			; Player Makes Move
			(format t "~%What is your move [row col]? ")
	
			(setf move (list (read) (read)))
		)
	)
	
)

; Switches opponent ID based on player ID
(defun opponent (player)
	(let((opponent 'W))
		(cond ((eq player 'W) (setf opponent 'B)))
		opponent
	)
)

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

(defun test_gen_succ (board player)
	(let ((states (gen_successors board player)))
		(dotimes (i (list-length states))
			(printBoard (nth i states))
		)
	) 
)

; Each place can result in mul
(defun gen_successors (board player)
	(let
		(
			(successors '())


		)
		(dotimes (i 64)
			(let
				(
					(row (+ (floor (/ i 8)) 1))
					(col (+ (mod i 8) 1))
				)
				(cond
					((eq (nth i board) '-)
						(setf temp (test_move (copy-list board) (list row col) player))
						(cond((not (eq temp nil))
							(printBoard temp)
							(setf successors (cons temp successors)))
						)
					) 
				)
			)
		)
		(cond((not(eq successors nil))
			successors)

			(t nil)
		)
	)
)

(defun make-move ())

(defun evaluation ())

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


(defun test_move (board pos plyr)
"
Usage: (test_move board pos plyr)

board - 8x8 board represented by a 64 element list
pos - a list consisting of two elements: (row column)
plyr - 'B or 'W representing the current player 

"
	(let
		(
			(newboard board)
			(opp (opponent plyr))
			(pos_row (- (car pos) 1)) ; alt -> (floor (/ pos 8))
			(pos_col (- (cadr pos) 1)) ; alt -> (mod pos 8)
			(position (+ (- (cadr pos) 1) (* (- (car pos) 1) 8)) )
		)
		(cond ((eq (nth position board) '-)
			(dotimes (row 3)
				(dotimes (col 3)
					(setf new_row (+ pos_row (- row 1)))
					(setf new_col (+ pos_col (- col 1)))
					;(format t "ROW: ~d  COL: ~d~%" new_row new_col)
					(cond ((and (< new_row 8) (> new_row -1)
							(< new_col 8) (> new_col -1))
							(setf new_pos (+ new_col (* new_row 8))))
						(t (setf new_pos -1))
					)
					;(format t "NEW POSITION: ~d~%" new_pos)
					(cond ((and (> new_pos -1) (eq (nth new_pos board) opp))
						;(format t "FOUND AT: ~d~%" new_pos)
						(flip_pieces board position	(- col 1) (- row 1) plyr)
						)
					)
				)
			))
			(t (setf position nil))
		)
		(cond ((and (not (eq position nil)) (eq (nth position board) plyr)) 
			board)

			(t nil)
		)
	)
)


(defun flip_pieces (board pos x_vec y_vec player)
	(let*
		(
			(opp (opponent player))
			(x_pos (mod pos 8))
			(col_bound (and (< (+ x_pos x_vec) 8)
				(> (+ x_pos x_vec) -1)))
			(row_bound (and (< pos 64) (> pos -1)))
			(next_pos (+ pos x_vec (* y_vec 8)))
		)
		(cond((and col_bound row_bound)
			(cond 
				((eq (nth next_pos board) opp)
					(flip_pieces board next_pos x_vec y_vec player))
			)
			(cond
				((eq (nth next_pos board) player)
					(setf (nth pos board) player))
			)
		))
	)
)