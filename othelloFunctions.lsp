#|
                    ***** othelloFunctions.lsp *****



Author: Steve Huerta, Matthew Rames
Written Spring 2016 for CSC447/547 AI class.

This file contains functions for an othello game. Such as player moves and computer AI moves.
The AI uses mini-max with alpha beta pruning to determine the best moves based on a 4ply depth.
This file also contains methods for scoring a board, fliping, placing pieces on a board, and 
generating successors of a board state.

|#

;--------------------------------------------------------------------------

; This is the entry point of the othello game
; Depending on what the color the user chose either the computer or user will go first
(defun StartGame(playerColor)

	(let((playerMove nil))
		; Create the board
		(setf board (createBoard))
		(printBoard board)
		
		; Check if the human or AI goes first
		(cond
			((equalp playerColor 'B)
				(loop 
					; Termination test
					(when 	(or 
								(null (member '- board)) ; Check if board has no empty spots left
								(or (null (member 'W board)) (null (member 'B board))) ; Check if board has no B or W spots
							)
						(scoring_count board)
						(return board)
					)
					
					; Player Makes Move
					(setf board (playerTurn board 'B))
					
					(printBoard board)
					
					; Termination test
					(when 	(or 
								(null (member '- board)) ; Check if board has no empty spots left
								(or (null (member 'W board)) (null (member 'B board))) ; Check if board has no B or W spots
							)
						(scoring_count board)
						(return board)
					)
					
					; Computer Makes Move
					(setf board (make-move board 'W 4))
					
					(printBoard board)
				)
			)
			
			((equalp playerColor 'W)
				(loop 
					; Termination test
					(when 	(or 
								(null (member '- board)) ; Check if board has no empty spots left
								(or (null (member 'W board)) (null (member 'B board))) ; Check if board has no B or W spots
							)
						(scoring_count board)
						(return board)
					)
					
					; Computer B Makes Move
					(setf board (make-move board 'B 4))
					
					(printBoard board)
					
					; Termination test
					(when 	(or 
								(null (member '- board)) ; Check if board has no empty spots left
								(or (null (member 'W board)) (null (member 'B board))) ; Check if board has no B or W spots
							)
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

; This function handles a users turn in othello
(defun playerTurn (board playerColor)
	(cond 
		; Player has no moves
		((null (gen_successors board playerColor))
			(format t "~%Player ~s has no moves...~%" playerColor)
		)
		; Player can make a move
		((not (null (gen_successors board playerColor)))
			(setf playerMove nil)
			; While the player move is null keep prompting the user to make a move
			(loop while (null playerMove) do
				
				; Prompt user for row and col
				(format t "~%What is your move player ~s [row col]? " playerColor)

				(setf move (list (read) (read)))

				; Attempt to make a player move
				(setf playerMove (player_move board move playerColor))
				
				; If incorrect move display error message
				(when (null playerMove)
					(format t "~%Incorrect move, try again.~%" move)	
				)
			)
		)
	)
	board
)

; Function called for an AI's turn
(defun make-move (board playerColor ply)	
	(cond 
		; Player has no moves
		((null (gen_successors board playerColor))
			(format t "~%Player ~s has no moves...~%" playerColor)
		)
		; Player can make a move
		((not (null (gen_successors board playerColor)))
			(format t "~%~s players move...~%" playerColor)
			(setf board (nth 1 (diet_minimax board playerColor ply)))
		)
	)
	board
)

; Empty Othello Init
(defun othello-init ()

)

; Switches opponent ID based on player ID
(defun opponent (player)
	(let((opponent 'W))
		(cond ((eq player 'W) (setf opponent 'B)))
		opponent
	)
)

; This function switchs player type from max to min and vice versa
(defun switchPlayerType (type)
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

<<<<<<< HEAD

; Board is going to be a node - see node definition
(defun minimax (board player depth &optional (type 'max) (path '())
				(alpha -1000000) (beta 1000000))
	(let
		( 
			(moves (gen_successors board player))
			(min_value 1000000)
			(max_value -1000000)
			(next (o_type type))
			(opp (opponent player))
			(path (cons board path))
			(bestnode nil)
		)
		(cond 
			((or (eq depth 0)(eq moves nil))
				(setf node (make-node :state board :score (score_board board player type) :path path))
			)
			((not (null board))
				;(format t "DEPTH: ~d PLAYER: ~s~%" depth player)
				(dotimes (i (list-length moves))
					(cond ((> beta alpha)
						(setf node (minimax (nth i moves) opp (1- depth) next path alpha beta))
						(cond 
							((and (not (null node)) (eq type 'max) (< max_value (node-score node)))
								(setf max_value (node-score node))
								(cond ((< alpha max_value) (setf alpha max_value)))
								(setf bestnode node)
							)
							((and (not (null node)) (eq type 'min) (> min_value (node-score node)))
								(setf min_value (node-score node))
								(cond ((> beta min_value) (setf beta min_value)))
								(setf bestnode node)
							)
						))
						;(t (format t "CUT PLAY: ~d DEPTH: ~d PLAYER: ~s TYPE: ~s~%" i depth player type))
					)
				)
				bestnode 
			)
		)
	)
)

; This function will initialize a minimax with alpha beta 
; pruning algorithm. The player calling this function will 
; be the maximizing player. The function will recursively 
; call itself until the desired n-ply depth is reached and 
; the board state will be scored relative to the maximizing
; player.
;
; PARAMETERS:
;
; board - state of the board
; player - player 'B or 'W
; depth - the current depth of the minimax play
; type - minimizing or maximizing player ('max or 'min)
; alpha - the best value for maximum play
; beta - the best value for minimum play
;
; RETURNS:
; 
; (score board)
=======
>>>>>>> 72a9e9dc0c8e11e0ed6357136377f0af298deaa4
(defun diet_minimax (board player depth &optional (type 'max) 
				(alpha -1000000) (beta 1000000))
	(let
		( 
			(moves (gen_successors board player))
			(min_value 1000000)
			(max_value -1000000)
			(next (switchPlayerType type))
			(opp (opponent player))
			(beststate nil)
		)
		(cond 
			((or (eq depth 0)(eq moves nil))
				(setf score (list (score_board board player type) board))
			)
			((not (null board))
				(dotimes (i (list-length moves))
					(setf child (diet_minimax (nth i moves) opp (1- depth) next alpha beta))
					(setf score (car child))
					(cond 
						((and (not (null score)) (eq type 'max) (> score max_value))
							(setf max_value score)
							(setf beststate (list score (nth i moves)))
							(cond ((> score alpha) (setf alpha score)))
							(cond ((<= beta alpha) (return beststate)))
						)
						((and (not (null score)) (eq type 'min) (< score min_value))
							(setf min_value score)
							(setf beststate (list score (nth i moves)))
							(cond ((< score beta) (setf beta score)))
							(cond ((<= beta alpha) (return beststate)))
						)
					) 
				)
				beststate
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
		
		(when (> black white)
			(format t "~%black player won!~%")
		)
					
		(when (< black white)
			(format t "~%white player won!~%")
		)
		(format t "player b score: ~d~%" black)
		(format t "player w score: ~d~%" white)
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
		;(printBoard board)
		(dotimes (i 64)
			(setf temp (nth i board))
			(cond 
				((eq temp 'B) (setf black (1+ black)))
				((eq temp 'W) (setf white (1+ white)))
			)
		)
		;(format t "WHITE: ~d BLACK: ~d~%" white black)
		(cond
			((eq player 'B) black)
			(t white) 
		)
	)
)



<<<<<<< HEAD

(setf testboard '(B W B B W B B B 
   				  B B W B W B B B
				  W W B B B B B B
				  W W W B B B W B
				  W W B W B B W B 
				  W B W B B B W W 
				  W W B B B W B W 
				  W W W W W B W - )
 )



(defun find_move (board next_board)
	(dotimes (i 64)
		(cond 
			((and (eq (nth i board) '-)(not (eq (nth i next_board) '-)))
				(format t "POS: ~d~%" i)
				(return (list (+ (floor(/ i 8)) 1) (+ (mod i 8) 1)))
			)
		)
	)
)

(setf second '(- - - - - - - - - - - - - - - - - - - - - - - - - - W W B - - - - - - B W - -
 - - - - - - - - - - - - - - - - - - - - - - - - -))
=======
>>>>>>> 72a9e9dc0c8e11e0ed6357136377f0af298deaa4
