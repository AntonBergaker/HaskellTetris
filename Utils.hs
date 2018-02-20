module Utils( turn, overlap, lineFull, lineEmpty, isVoid) where

import TetrisTypes
import Data.List

{- rotate grid
	returns the given grid rotated clockwise
-}
turn :: Grid -> Grid
turn grid = (map reverse . transpose) grid


{- overlap board piece offset
	Checks if a given piece collides with the grid at the given offset and returns true if it does
-}
overlap :: Grid -> Grid -> Position -> Bool
overlap field piece (xOff, yOff) = overlap' (drop (round yOff) field) piece (round xOff)
	where
		overlap' :: Grid -> Grid -> Int -> Bool
		overlap' [] _ _ = False
		overlap' _ [] _ = False
		overlap' (x:xs) (y:ys) xOff = if (overlapRow (drop xOff x) y)
			then True
			else overlap' xs ys xOff


overlapRow :: [Block] -> [Block] -> Bool
overlapRow [] _ = False
overlapRow _ [] = False
overlapRow (Block _:_) (Block _:_) = True
overlapRow (_:xs) (_:ys) = overlapRow xs ys


isVoid :: Block -> Bool
isVoid (Block _) = False
isVoid Void = True
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
