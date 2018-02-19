

-- | Display "Hello World" in a window.
--
import TetrisTypes
import Graphics.Gloss

main
 = display
        (InWindow
	       "Hello World" 	 -- window title
		(400, 150) 	 -- window size
		(10, 10)) 	 -- window position
	white			 -- background color
	picture			 -- picture to display

picture
	= Translate (-170) (-20) -- shift the text to the middle of the window
	$ Scale 0.5 0.5		 -- display it half the original size
	$ Text "Hello World"	 -- text to display


{- fall
	Checks if a piece can be moved down and if it can't returns a new piece and the old piece applied to the grid
-}
fall :: Grid -> Grid -> Position -> (Grid -> Grid -> Position)
fall board piece offset = undefined;


{- applyMove
	Checks if a piece can be moved the given movement and returns a new offset
-}
applyMove :: Grid -> Grid -> Position -> Int -> Position
applyMove board piece offset movement = undefined;

{- canMove 
	Checks if a piece can be moved the indicated movement
-}
canMove :: Grid -> Grid -> Position -> Int -> Boolean
canMove board piece offset movement = undefined;


{- applyRotate 
	Checks if you can rotate and if you can return the rotated piece
-}
applyRotate :: Grid -> Grid -> Position -> Grid
applyRotate board piece offset = undefined;


{- canRotate
	Checks if a piece can rotate at a given position
-}
canRotate :: Grid -> Grid -> Position -> Boolean
canRotate board piece offset = undefined;


{- linesCleared board
	Returns a board where if any lines are full they are cleared and gives how many lines were cleared
-}
linesCleared :: Grid -> (Grid, Int)
linesCleared board = undefined;