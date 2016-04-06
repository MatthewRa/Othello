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

; Each place can result in mul
(defun gen_moves (game_node depth)
	(cond 
		((eq depth 0)
			(scoreboard game_node)
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

(defun test_move (board pos plyr)
	(let
		(
			(newboard board)
			(opp (opponent plyr))
			(pos_row (floor (/ pos 8)))
			(pos_col (mod pos 8))
		)
		(dotimes (row 3)
			(dotimes (col 3)
				(setf new_row (+ pos_row (- row 1)))
				(setf new_col (+ pos_col (- col 1)))
				(cond ((and (< new_row 8) (> new_row -1)
						(< new_col 8) (> new_col -1))
						(setf new_pos (+ new_col (* new_row 8))))
					(t setf new_pos -1)
				)
				(cond
					((and (> new_pos -1) (eq (nth new_pos board) opp))
						(setf newboard (flip_pieces board new_pos 
							(- col 1) (- row 1) plyr ))
						(cond
							((not (eq newboard NIL)) (setf board newbaord))
					)	)
				)
			)
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
		board 

	)
)