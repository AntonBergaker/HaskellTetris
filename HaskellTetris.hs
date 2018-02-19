--module HaskellTetris(fall, applyMove, applyRotate, linesCleared) where

import TetrisTypes
import Utils
import Graphics.Gloss
import Test.HUnit


{- fall
	Checks if a piece can be moved down and if it can't returns a new piece and the old piece applied to the grid otherwise returns a new offset where the piece has been moved 1 step
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
canMove :: Grid -> Grid -> Position -> Int -> Bool
canMove board piece offset movement = undefined;


{- applyRotate 
	Checks if you can rotate and if you can return the rotated piece
-}
applyRotate :: Grid -> Grid -> Position -> Grid
applyRotate board piece offset = undefined;


{- canRotate
	Checks if a piece can rotate at a given position
-}
canRotate :: Grid -> Grid -> Position -> Bool
canRotate board piece offset = undefined;


{- linesCleared board
	Returns a board where if any lines are full they are cleared and gives how many lines were cleared
-}
linesCleared :: Grid -> (Grid, Int)
linesCleared board = undefined;




 -- Tests a simple rotation of a small grid
test1 = TestCase $ assertEqual "rotate"
	([ [ Void, Block red ], [Void, Block red] ]) ([ [ Block red, Block red ], [Void, Void] ])


runtests = runTestTT $ TestList [test1]