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
					(setf newMove (make-move board 'W 4))
					(when (not (null newMove))
						(setf board(player_move board newMove 'W))
					)
					
					
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
					(setf newMove (make-move board 'B 4))
					(when (not (null newMove))
						(setf board(player_move board newMove 'B))
					)
					
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
			(setf NewBoard (nth 1 (minimax board playerColor ply)))
			(setf move (find_move board NewBoard))
			(format t "~%~s players moved at: ~d ~d~%" playerColor (nth 0 move) (nth 1 move))
		)
	)
	move
)

; Empty Othello Init - DO WE NEED THIS????????????????????????
(defun othello-init ()

)

; Switches opponent ID based on player ID
; PARAMETERS: 
; player - player piece 'B or 'W
(defun opponent (player)
	(let((opponent 'W))
		(cond ((eq player 'W) (setf opponent 'B)))
		opponent
	)
)

; This function switchs player type from max to min and vice versa
; PARAMETERS: 
; type - 'max or 'min
(defun switchPlayerType (type)
	(let ((other_type 'min))
		(cond ((eq type 'min) (setf other_type 'max)))
		other_type
	)
)

; This functions initializes board for othello
(defun createBoard ()
	; initially, all positions are empty
	(let ((board (make-list 64 :initial-element '-)))
		; set the initial positions
		(setf (nth 27 board) 'W)
		(setf (nth 36 board) 'W)
		(setf (nth 28 board) 'B)
		(setf (nth 35 board) 'B)
		; return the board
		board
	)
)

; Generate successive states from current state for a player
; Returns NIL if no moves are available
; Returns a list of lists (board states)
; PARAMETERS: 
; board - state of the board
; player - player piece 'B or 'W
(defun gen_successors (board player)
	(let
		(
			(successors '())
		)
		(dotimes (i 64)
			(let
				(
					; calculate our row and column position
					(row (+ (floor (/ i 8)) 1))
					(col (+ (mod i 8) 1))
				)
				(cond
					; If we come across an empty position...
					((eq (nth i board) '-)
						; Try to make a play...
						(setf temp (player_move (copy-list board) (list row col) player))
						; If we can make a play...
						(cond((not (eq temp nil))
							; Then this is a successor and we'll add it to our list
							(setf successors (cons temp successors)))
						)
					) 
				)
			)
		)
		; if we have no successors we just return nil, otherwise
		; we return a list of successors
		(cond((not(eq successors nil))
			successors)

			(t nil)
		)
	)
)

; print board state
; PARAMETERS: 
; board - state of the board
(defun printBoard (board)
	; print column headings
	(format t "  1 2 3 4 5 6 7 8~%")
	; iterate across the positions
	(dotimes (i 64)
		(cond
			; if we are the last column carry a new line
			((eq 7 (mod i 8))
				(format t "~s~%" (nth i board)))
			; if we are starting a new row, print the row heading
			((eq 0 (mod i 8))
				(format t "~d ~s " (1+ (floor (/ i 8))) (nth i board)))
			; otherwise print the board value
			(t (format t "~s " (nth i board)))
		)
	)
)

; This function attempts to place a piece on the board
; if the play is valid, the function will return a 
; board state with the play. Otherwise it will 
; return nil.
; PARAMETERS: 
; board - state of the board
; pos - position of the play in (row col) list
; plyr - player piece 'B or 'W
(defun player_move (board pos plyr)
	(let
		(
			; create a copy of our board
			(newboard board)
			; name our opponent
			(opp (opponent plyr))
			; get our row position  
			(pos_row (- (car pos) 1)) 
			; get our col position 
			(pos_col (- (cadr pos) 1)) 

			; calculate the position of play in the 1D list
			(position (+ (- (cadr pos) 1) (* (- (car pos) 1) 8)) )
		)
		; if the play is on an empty location
		(cond ((eq (nth position board) '-)
			; lets check neighbors to see if it is valid
			(dotimes (row 3)
				(dotimes (col 3)
					(setf new_row (+ pos_row (- row 1)))
					(setf new_col (+ pos_col (- col 1)))
					; check the bounds of possible neighbor 
					(cond ((and (< new_row 8) (> new_row -1)
							(< new_col 8) (> new_col -1) 
							(not (and (eq new_row 0) (eq new_col 0))))
							; and if valid, mark as a position to try
							(setf new_pos (+ new_col (* new_row 8))))
						; or its an invalid move
						(t (setf new_pos -1))
					)
					; if not an invalid position and belongs to the opponent
					; let's send it over to flip pieces to see if it is a 
					; valid move
					(cond ((and (> new_pos -1) (eq (nth new_pos board) opp))
						(flip_pieces board position	(- col 1) (- row 1) plyr)
						)
					)
				)
			))
			; otherwise, its not a valid location to make a move
			(t (setf position nil))
		)
		; if we had a move, return the board, otherwise return nil
		(cond ((and (not (eq position nil)) (eq (nth position board) plyr)) 
			board)

			(t nil)
		)
	)
)

; This function will flip the pieces of the board if 
; a terminal player piece is found on the opposite 
; side of the play.
; PARAMETERS:
; board - state of the board
; pos - position of the play
; x_vec - x vector (-1 0 1)
; y_vec - y vector (-1 0 1)
; player - player piece 'B or 'W
; 
; RETURNS: new state of the board
(defun flip_pieces (board pos x_vec y_vec player)
	(let*
		(
			(opp (opponent player)) ; opponent piece 
			(x_pos (mod pos 8)) ; get column position
			; check bounds of position when x vector is applied
			(col_bound (and (< (+ x_pos x_vec) 8)
				(> (+ x_pos x_vec) -1)))
			; check row is valid
			(row_bound (and (< pos 64) (> pos -1)))
			; calc next position
			(next_pos (+ pos x_vec (* y_vec 8)))
			; check position is in bounds after move 
			(next_bound (and (< next_pos 64) (> next_pos -1)))
		)
		; if we are still in bounds
		(cond((and col_bound row_bound next_bound)
			(cond 
				; if the next piece is an opposing piece
				((eq (nth next_pos board) opp)
					; call ourselves again to check
					(flip_pieces board next_pos x_vec y_vec player))
			)
			(cond
				; We're back from recursion and we are going
				; to flip pieces as we recurse back to our position
				((eq (nth next_pos board) player)
					(setf (nth pos board) player))
			)
		))
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
; score - the score of the maximizing player
; board - board state
(defun minimax (board player depth &optional (type 'max) 
				(alpha -1000000) (beta 1000000))
	(let
		( 
			; create a list of all successive states
			(moves (gen_successors board player)) 
			; set min value for local comparison
			(min_value 1000000)
			; set max value for local comparison
			(max_value -1000000)
			; determine next play type ('max or 'min)
			(next (switchPlayerType type))
			; determine opposing player
			(opp (opponent player))
			; intialize return value to null
			(beststate nil)
		)
		(cond 
			; if terminal node or reached appropriate ply depth
			((or (eq depth 0)(eq moves nil))
				; score the board and return the board state
				(setf score (list (score_board board player type) board))
			)
			; otherwise and board is not null
			((not (null board))
				; iterate across the number of board successors
				(dotimes (i (list-length moves))
					; get the score and board from minimax
					(setf child (minimax (nth i moves) opp (1- depth) next alpha beta))
					; unpack the score from the returned list
					(setf score (car child))
					(cond 
						; if we are the maximizing player and found a higher score
						((and (not (null score)) (eq type 'max) (> score max_value))
							; set this value to our new max
							(setf max_value score)
							; create a list with this value
							(setf beststate (list score (nth i moves)))
							; if greater than alpha, set to our new alpha
							(cond ((> score alpha) (setf alpha score)))
							; if less than beta, stop searching on this branch
							(cond ((<= beta alpha) (return beststate)))
						)
						; if we are the minimizing player and found a lower score
						((and (not (null score)) (eq type 'min) (< score min_value))
							; set this value to our new min
							(setf min_value score)
							; create a list with this value
							(setf beststate (list score (nth i moves)))
							; if less than beta, set to our new beta
							(cond ((< score beta) (setf beta score)))
							; if less than beta, stop searching on this branch
							(cond ((<= beta alpha) (return beststate)))
						)
					) 
				)
				; return (score board)
				beststate
			)
		)
	)
)

; Scoring count will output the board state to 
; the console for both W and B players, and 
; announce the winner of the game based on 
; the comparison of values between players
(defun scoring_count (board)
	(let
		(
			; initialize our scoring to 0
			; for both players
			(black 0)
			(white 0)
		)
		; iterate across the board
		(dotimes (i 64)
			(setf temp (nth i board))
			; sum up totals for both players
			(cond 
				((eq temp 'B) (setf black (1+ black)))
				((eq temp 'W) (setf white (1+ white)))
			)
		)
		
		; Compare scores between players and 
		; announce a winner
		(when (> black white)
			(format t "~%black player won!~%")
		)
					
		(when (< black white)
			(format t "~%white player won!~%")
		)

		(when (eq black white)
			(format t "~%it's a draw!~%")
		)

		; Output scores
		(format t "player b score: ~d~%" black)
		(format t "player w score: ~d~%" white)
		(list black white)  
	)
)

; This function will score the board for the maximizing player
; and return that score.
(defun score_board (board player type)
	(let
		(
			; Determine the maximizing player
			(player (cond 
				((eq type 'max) player)
				(t (opponent player))))
			; initialize our sums for both to 0
			(white 0)
			(black 0)
		)
		; iterate across the board
		(dotimes (i 64)
			(setf temp (nth i board))
			; Sum up the scores for black and white
			(cond 
				((eq temp 'B) (setf black (1+ black)))
				((eq temp 'W) (setf white (1+ white)))
			)
		)
		; return the score for the maximizing player
		(cond
			((eq player 'B) black)
			(t white) 
		)
	)
)


; To determine what move was selected, we compare the previous state
; of the board to the state of the board after a play was selected.
; This function will return the (row column) of the play as a list.
(defun find_move (board next_board)
	; search across all board posiitions
	(dotimes (i 64)
		(cond 
			; if the position is empty on board and not empty on the
			; next board
			((and (eq (nth i board) '-)(not (eq (nth i next_board) '-)))
				; that's the change, and lets return a list containing
				; the (row column)
				(return (list (+ (floor(/ i 8)) 1) (+ (mod i 8) 1)))
			)
		)
	)
)
