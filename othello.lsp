#|
                    ***** othello.lsp *****



Author: Steve Huerta, Matthew Rames
Written Spring 2016 for CSC447/547 AI class.

This file contains the main function for othello as well as a function to handle
starting another game. Othello can be started via command line or from inside 
CLISP read eval print loop, see below for examples. 

Command Line:
	clisp othello.lsp (Black or White)
	
Inside CLISP
	(load 'othello)
	(othello) OR (othello "Black")
				 (othello "White")


|#

;--------------------------------------------------------------------------

(load 'othelloFunctions)

; This function will prompt the user if they would like to play another game of othello.
(defun playAgain ()
	; Prompt user if the would like to play again
	(format t "~%~%Would you like to play again? [y/n]? ")
	
	; Read input 
	(setf again (read))
	
	; Start another game of othello if the player says yes
	(when (equalp again 'y)
		(othello)
	)
	
	; Exit game if player doesn't want to play again.
	(when (equalp again 'n)
		(format t "Thanks for playing!~%")
		(exit)
	)
)

;Starting point of othello program
(defun othello ( &optional args )
	(cond
		; Othello was started in clisp repl as (othello "Black/White")
		((null (listp args))
			(print args)
			(setf playerColor args)
		)
		
		; Othello was started in command line given a color
		((= (length args) 1)
			(when (listp args)
				(setf playerColor (car args))
			)
		)
		
		; Othello was started in clisp repl as (othello)
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
	
	; Check chosen player color and output correct statement to user
	
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
	
	; Begin Othello
	(StartGame playerColor)
	
	; Prompt user to player again
	(playAgain)
)

; call othello if parameters were passed in
(when (= (length *ARGS*) 1)
	(othello *ARGS*)
)