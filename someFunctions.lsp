(defstruct node
	state ; 8x8 representation of board
	player ; current player
	black ; number of black pieces on board
	white ; number of white pieces on board
	score ; the score of the player W or B
	path ; list of lists to get to this state
)

(defun othello-init ()

)

(defun StartGame(playerColor)

	(let((playerMove nil))
		(setf board (createBoard))
		(printBoard board)
		
		(cond
			((equalp playerColor 'B)
				(loop 
					; Termination test
					(when(null (member '- board))
						(scoring_count board)
						(return board)
					)
					
					; Player Makes Move
					(setf board (playerTurn board 'B))
					
					(printBoard board)
					
					; Termination test
					(when(null (member '- board)) 
						(scoring_count board)
						(return board)
					)
					
					; Computer Makes Move
					; Check if player W can make a move
					(setf board (playerTurn board 'W))
					;(make-move board 'W 4)
					
					(printBoard board)
				)
			)
			
			((equalp playerColor 'W)
				(loop 
					; Termination test
					(when(null (member '- board))
						(scoring_count board)
						(return board)
					)
					
					; Computer B Makes Move
					(setf board (playerTurn board 'B))
					;(make-move board 'B 4)
					
					(printBoard board)
					
					; Termination test
					(when(null (member '- board))
						(scoring_count board)
						(return board)
					)
					
					; Player W Makes Move
					(setf board (playerTurn board 'W))
					
					(printBoard board)
				)
			)
		)
	)
	
)

(defun playerTurn (board playerColor)
	(cond 
		; Player has no moves
		((null (gen_successors board playerColor))
			(format t "~%Player ~s has no moves...~%" playerColor)
		)
		; Player can make a move
		((not (null (gen_successors board playerColor)))
			(setf playerMove nil)
			(loop while (null playerMove) do
				
				(format t "~%What is your move player ~s [row col]? " playerColor)

				(setf move (list (read) (read)))

				(setf playerMove (player_move board move playerColor))
				
				(when (null playerMove)
					(format t "~%Incorrect move, try again.~%" move)	
				)
			)
		)
	)
	
	board
)

; Switches opponent ID based on player ID
(defun opponent (player)
	(let((opponent 'W))
		(cond ((eq player 'W) (setf opponent 'B)))
		opponent
	)
)

(defun o_type (type)
	(let ((other_type 'min))
		(cond ((eq type 'min) (setf other_type 'max)))
		other_type
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

(defun make-move (board playerColor ply)

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
	&optional (min 100000000) (max -1000000000) (path '()) (type 'max))
	(setf moves (gen_successors board player))
	(cond
		((or (eq depth 1) (null moves))
			(setf score (scoring_count board))
			(format t "SCORING @ DEPTH: ~d~%" depth)
			(setf node (make-node :state board :player player 
				:black (car score) :white (cadr score) 
				:score (score_board board player type) :path path))
		)
		(t (let
			(
				(num_moves (list-length moves))
				(opp (opponent player))
				(path (cons board path))
			)
			(dotimes (i num_moves)
				(setf node (minimax_ab (nth i moves) 
					opp (- depth 1) min max path (o_type type)))
				(setf temp_score (node-score node))
				(format t "temp score: ~d black: ~d~%" temp_score (node-score node))
				(format t "DEPTH: ~d~%" depth)
				(printBoard (nth i moves))
				; (cond 
				; 	((eq player 'B)
				; 		(setf temp_score (node-black node)))
				; 	(t (setf temp_score (node-white node)))
				; )
				(cond
					((> min temp_score)
						(setf min temp_score)
						(setf bestnode node)
					)
					((< max temp_score)
						(setf max temp_score)
						(setf bestnode node)
					)
				)
			)
			bestnode
			;(format t "NODE SCORE - BLACK: ~d  WHITE: ~d DEPTH: ~d~%" (node-black node) (node-white node) depth)
		))
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
		
		; (when (> black white)
		; 	(format t "~%Black player won!~%")
		; )
					
		; (when (< black white)
		; 	(format t "~%White player won!~%")
		; )
		; (format t "Player B Score: ~d~%" black)
		; (format t "Player W Score: ~d~%" white)
		(list black white)
	)
)

(defun score_board (board player type)
	(let
		(
			(player (cond 
				((eq type 'max) player)
				(t (opponent player))))
			(white 0)
			(black 0)
		)
		(dotimes (i 64)
			(setf temp (nth i board))
			(cond 
				((eq temp 'B) (setf black (1+ black)))
				((eq temp 'W) (setf white (1+ white)))
			)
		)
		(printBoard board)
		(cond
			((eq player 'B) black)
			(t white) 
		)
	)
)




(setf testboard '(B W B B W B B B 
   				  B B W B W B B B
				  W W B B B B B B
				  W W W B B B W B
				  W W B W B B W B 
				  W B W B B B W W 
				  W W B B B W B W 
				  W W W W W B W - ))
