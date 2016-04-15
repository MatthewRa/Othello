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
			(loop 
				; Termination test
				(when(null (member '- board)) return)
				
				; Player Makes Move
				;Check if player B can make a move
				(cond 
					; Player has no moves
					((null (gen_successors board playerColor))
						(format t "~%Player B has no moves... ")
					)
					; Player can make a move
					((not (null (gen_successors board playerColor)))
						(format t "~%What is your move player B [row col]? ")
		
						(setf move (list (read) (read)))
						
						(format t "~%Your move: ~A~%" move)
						(player_move board move 'B)
					)
				)

				
				(printBoard board)
				
				; Computer Makes Move
				; Check if player W can make a move
				(cond 
					; Player has no moves
					((null (gen_successors board playerColor))
						(format t "~%Player W has no moves... ")
					)
					; Player can make a move
					((not (null (gen_successors board playerColor)))
						(format t "~%What is your move player W [row col]? ")
						
						(setf move (list (read) (read)))
						
						(format t "~%Your move: ~A~%" move)
						(player_move board move 'W)
					)
				)
				
				(printBoard board)
			)
		)
		
		((equalp playerColor 'W)
			(loop 
				; Termination test
				(when(null (member '- board)) return)
				
				; Computer Makes Move
				;Check if player B can make a move
				(cond 
					; Player has no moves
					((null (gen_successors board playerColor))
						(format t "~%Player B has no moves... ")
					)
					; Player can make a move
					((not (null (gen_successors board playerColor)))
						(format t "~%What is your move player B [row col]? ")
		
						(setf move (list (read) (read)))
						
						(format t "~%Your move: ~A~%" move)
						(player_move board move 'B)
					)
				)
				
				(printBoard board)
				
				; Player Makes Move
				; Check if player W can make a move
				(cond 
					; Player has no moves
					((null (gen_successors board playerColor))
						(format t "~%Player W has no moves... ")
					)
					; Player can make a move
					((not (null (gen_successors board playerColor)))
						(format t "~%What is your move player W [row col]? ")
						
						(setf move (list (read) (read)))
						
						(format t "~%Your move: ~A~%" move)
						(player_move board move 'W)
					)
				)
				
				(printBoard board)
			)
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

; Just to get some formatted board states
(defun test_gen_succ (board player)
	(let ((states (gen_successors board player)))
		(dotimes (i (list-length states))
			(printBoard (nth i states))
		)
	) 
)

; testing minimax, seems to have issues with depths greater than nine....
; more troubleshooting to come
; still need to adjust to pass only the next move rather than best game state at nth depth
(defun test_minimax (board player depth)
	(setf result (get_max board player (- depth 1)))
)

; Generate successive states from current state for a player
; Returns NIL if no moves are available
; Returns a list of lists (board states)
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
						;(format t "TESTING ROW: ~d COL: ~d~%" row col )
						(setf temp (player_move (copy-list board) (list row col) player))
						(cond((not (eq temp nil))
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


(defun player_move (board pos plyr)
"
Usage: (player_move board pos plyr)

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
							(< new_col 8) (> new_col -1) 
							(not (and (eq new_row 0) (eq new_col 0))))
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
			(next_bound (and (< next_pos 64) (> next_pos -1)))
		)
		(cond((and col_bound row_bound next_bound)
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


; Board is going to be a node - see node definition
(defun minimax_ab(board player depth 
	&optional (min 100000000) (max -1000000000) ())
	(cond
		((eq depth 0)
			(setf score (scoring_count board))
			(setf node (make-node :state board :player player :black (car score) :white (cadr score)))
			(list node)
		)
		(t (let* 
			(
				(moves (gen_successors board player))
				(num_moves (cond 
					((eq moves nil) 0)
					(t (list-length moves)))
				(opp (opponent player))
			)




		))







	)
)




(defun get_max(board player depth)
	(cond 
		((eq depth 0)
			(setf score (scoring_count board))
			(setf node (make-node :state board :player player :black (car score) :white (cadr score)))
			; (format t "I'm at the MAX BLACK: ~vd WHITE: ~vd~%" 2 (node-black node) 2 (node-white node))
			; (format t "I'm at the MAX and depth 0~%")
			; (printBoard board)
			(list node)
		)
		(t 
			(let*
				(
					(maxval -10000000) ; negative infinity
					(moves (gen_successors board player))
					(highest 10000000)
					(num_moves 
						(cond((eq moves nil) 0)
							(t (list-length moves))
						)
					)
				) 
				(dotimes (i num_moves)
					;(format t "DEPTH:~vd MAXVALUE: ~vd HIGHEST: ~vd~%" 2 depth 2 maxval 2 highest)
					;(cond (and T T) ;(< maxval highest)
					(setf node (car (get_min (nth i moves) (opponent player) (- depth 1))))
					(setf highest (- (+ (node-white node) (node-black node)) 1))
					(cond 
						((eq player 'B)
							(cond ((< maxval (node-black node))
								(setf maxval (node-black node))
								(setf bestnode (list node (nth i moves) depth))
							)))
						((eq player 'W)
							(cond ((< maxval (node-white node))
								(setf maxval (node-white node))
								(setf bestnode (list node (nth i moves) depth))
							)))
						
					)
				)
				bestnode
			)
		)
	)
)


; Board is going to be a node - see node definition
(defun get_min(board player depth)
	(cond 
		((eq depth 0)
			(setf score (scoring_count board))
			(setf node (make-node :state board :player player :black (car score) :white (cadr score)))
			; (format t "I'm at the MIN BLACK: ~vd WHITE: ~vd~%" 2 (node-black node) 2 (node-white node))
			; (printBoard board)
			(list node)
		)
		(t 
			(let*
				(
					(minval 10000000) ; positive infinity
					(moves (gen_successors board player))
					(lowest 1)
					(num_moves 
						(cond((eq moves nil) 0)
							(t (list-length moves))
						)
					)
				) 
				(dotimes (i num_moves)
					;(format t "DEPTH:~vd MINVALUE: ~vd LOWEST: ~vd~%" 2 depth 2 minval 2 lowest)
					;(cond (and T T)	; > minval lowest
					(setf node (car (get_min (nth i moves) (opponent player) (- depth 1))))
					(cond 
						((eq player 'B)
							(cond ((> minval (node-black node))
								(setf minval (node-black node))							
								(setf bestnode (list node (nth i moves) depth))
							)))
						((eq player 'W)
							(cond ((> minval (node-white node))
								(setf minval (node-white node))
								(setf bestnode (list node (nth i moves) depth))
							)))
						
					)
				)
				bestnode
			)
		)
	)
)

(defun scoring_count (board)
	(let
		(
			(black 0)
			(white 0)
		)
		(dotimes (i 64)
			(setf temp (nth i board))
			(cond 
				((eq temp 'B) (setf black (1+ black)))
				((eq temp 'W) (setf white (1+ white)))
			)
		)
		(list black white)
	)
)


(setf testboard '(B W - B - - B - 
   				  - B - B - - B -
				  W W B B B B B B
				  W W W B B B W B
				  W W B W B B W B 
				  W B W B B B W W 
				  W W B B B - - - 
				  W W W W W - - - ))