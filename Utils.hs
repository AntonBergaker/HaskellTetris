    module Utils( turn, overlap, lineFull, lineEmpty, isVoid) where
    
    import TetrisTypes
    import Data.List
    
    {- rotate grid
        Rotates the supplied grid clockwise
        PRE: True
        RETURNS: The supplied grid rotated clockwise
        EXAMPLE: turn [ [ Block red, Block red ], [Void, Void] ] = [ [ Void, Block red ], [Void, Block red] ]
    -}
    turn :: Grid -> Grid
    turn grid = (map reverse . transpose) grid
    
    
    {- overlap board piece offset
        Checks if a given piece collides with the grid at the given offset and returns true if it does
        PRE: Offset does not have a negative y value
        RETURNS: True if the grids are in collision, False if they are not
        INVARIANT: Size of the grid
        EXAMPLE: overlap ([[Void, Void],  [Block blue, Block blue]])  ([[Void, Block blue],  [Void, Void]]) (0,1) = True
    -}
    overlap :: Grid -> Grid -> Position -> Bool
    overlap field piece (xOff, yOff) = overlap' (drop (round yOff) field) piece (round xOff)
        where
            overlap' :: Grid -> Grid -> Int -> Bool
            overlap' [] _ _ = False
            overlap' _ [] _ = False
            overlap' (x:xs) (y:ys) xOff = 
                if (xOff < 0) then
                    if (overlapRow (x) (drop (-xOff) y))
                        then True
                        else overlap' xs ys xOff
                else 
                    if (overlapRow (drop xOff x) y)
                        then True
                        else overlap' xs ys xOff
    
    
    {- overlapRow row1 row2
        Checks if two rows have overlapping blocks
        PRE: True
        RETURNS: True if the rows have collisions, False if they do not
        INVARIANT: Size of the rows
        EXAMPLE: overlapRow [Block red, Void] [Block red, Void] = True
    -}
    overlapRow :: [Block] -> [Block] -> Bool
    overlapRow [] _ = False
    overlapRow _ [] = False
    overlapRow (Block _:_) (Block _:_) = True
    overlapRow (_:xs) (_:ys) = overlapRow xs ys
    
    
    {- isVoid block
        Checks if a block is void or filled
        PRE: True
        RETURNS: True if the block is void, False if it is filled
        EXAMPLES: isVoid (Block red) = False,  isVoid Void = True
    -}
    isVoid :: Block -> Bool
    isVoid (Block _) = False
    isVoid Void = True
    
    
    {- lineFull line
        Checks if a line is completely filled with blocks where no blocks are void
        PRE: True
        RETURNS: True if there exists at least one void in the line
        INVARIANT: Size of the line
        EXAMPLE: lineFull [Block red, Block red, Block blue] = True
    -}
    lineFull :: [Block] -> Bool
    lineFull [] = True;
    lineFull (Block _:xs) = lineFull xs;
    lineFull (Void:xs) = False;
    
    {- lineEmpty line
        Checks if a line is completely empty of blocks
        PRE: True
        RETURNS: True if there are no non-voided blocks in the line. Otherwise False
        INVARIANT: Size of the line
        EXAMPLE: lineEmpty [Block red, Void, Void] = False
    -}
    lineEmpty :: [Block] -> Bool
    lineEmpty [] = True;
    lineEmpty (Void:xs) = lineEmpty xs;
    lineEmpty (Block _:xs) = False;
    