module Utils( rotate, overlap, lineFull, lineEmpty) where

import TetrisTypes


{- rotate grid
	returns the given grid rotated clockwise
-}
rotate :: Grid -> Grid
rotate grid = undefined;

{- overlap board piece offset
	Checks if a given piece collides with the grid at the given offset and returns true if it does
-}
overlap :: Grid -> Grid -> Position -> Bool
overlap field piece offset = undefined;

{- lineFull line
	Returns true if the line is completely filled with blocks
-}
lineFull :: [Block] -> Bool
lineFull [] = True;
lineFull (Block _:xs) = lineFull xs;
lineFull (Void:xs) = False;

{- lineEmpty line
	Returns true if the line is completely empty of blocks
-}
lineEmpty :: [Block] -> Bool
lineEmpty [] = True;
lineEmpty (Void:xs) = lineEmpty xs;
lineEmpty (Block _:xs) = False;