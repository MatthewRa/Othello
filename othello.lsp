#|
                    ***** othello.lsp *****



Author: Steve Huerta, Matthew Rames
Written Spring 2016 for CSC447/547 AI class.

|#

;--------------------------------------------------------------------------

(load 'othelloFunctions)

(defun playAgain ()
	; Prompt user if the would like to play again
	(format t "~%~%Would you like to play again? [y/n]? ")
	
	(setf again (read))
	
	(when (equalp again 'y)
		(othello)
	)
	
	(when (equalp again 'n)
		(format t "Thanks for playing!~%")
		(exit)
	)
)

;Starting point of othello program
(defun othello ( &optional args )
	(cond
		((= (length args) 1)
			(setf playerColor (car args))
		)
		
		((= (length args) 0)
			; Propmpt player if they would like to go first or second
			(format t "Would you like to move first [y/n]? ")
			
			(setf response (read))
			
			(when (equalp response 'y)
				(setf playerColor "Black")
				
			)
			
			(when (equalp response 'n)
				(setf playerColor "White")
			)
		)
	)
	
	;(format t "Player Color: ~A~%" playerColor)
	
	(when (equalp playerColor "Black")
		(format t "~%OK! You will be playing Black. When asked for your move, please enter ") 
		(format t "the row and column in which you would like to place a Black stone. Remember, ")
		(format t "you must outflank atleast one White stone, or forfeit your move. ~%~%")
		(setf playerColor 'B)
	)
	
	(when (equalp playerColor "White")
			(format t "~%OK! You will be playing White. When asked for your move, please enter ")
			(format t "the row and column in which you would like to place a White stone. Remember, ")
			(format t "you must outflank atleast one Black stone, or forfeit your move. ~%~%")
			(setf playerColor 'W)
	)
	
	;Begin Othello
	(StartGame playerColor)
	
	(playAgain)
)

(when (= (length *ARGS*) 1)
	(othello *ARGS*)
)