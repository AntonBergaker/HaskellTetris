import TetrisTypes
import Utils
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Test.HUnit
import Pieces
import System.Random

main :: IO ()
main = play window background 1 newGameState render handleKeys update

window :: Display
window = InWindow "Tetris" (400, 640) (10, 10)

background :: Color
background = black

blockSize :: Float
blockSize = 32

boardBounds :: Position
boardBounds = (10,20)


newGameState :: GameState
newGameState = (emptyBoard, t, (5,0), 0, 0)

emptyBoard :: Grid
emptyBoard = replicate 20 $ replicate 10 Void

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyLeft ) Down _ _) (board, piece, offset, score, time) = (board, piece, newOffsetL, score, time)
	where
		newOffsetL = applyMove board piece offset (-1)
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) (board, piece, offset, score, time) = (board, piece, newOffsetR, score, time)
	where
		newOffsetR = applyMove board piece offset ( 1)
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) (board, piece, offset, score, time) = (board, newPiece, offset, score, time)
	where
		newPiece = applyRotate board piece offset
handleKeys _ gameState = gameState


update :: Float -> GameState -> GameState
update inc (board, piece, offset, score, time) = (newBoard, newPiece, newOffset, score, time)
	where
		(newBoard, newPiece, newOffset) = fall board piece offset

{- render gamestate
	Creates a picture from the given gamestate
-}
render :: GameState -> Picture
render (board, piece, (x,y), score, time) = pictures (boardPictures ++ piecePictures)
	where
		boardPictures = renderGrid board (0,0)
		piecePictures = renderGrid piece (x*blockSize, y*blockSize)

{- renderGrid grid position
	returns a list of pictures from the input grid and position
-}
renderGrid :: Grid -> Position -> [Picture]
renderGrid [] _ = []
renderGrid (r:rs) p@(x,y) = (renderRow r p) ++ (renderGrid rs (x,y+blockSize))

{- renderRow grid position
	returns a list of pictures from the given row and position
-}
renderRow :: [Block] -> Position -> [Picture]
renderRow [] _ = []
renderRow  (Void:bs)     (x,y) = renderRow bs (x+blockSize, y)
renderRow ((Block c):bs) (x,y) = color c ((translate (x-160) (320-y) (rectangleSolid blockSize blockSize))) : (renderRow bs (x+blockSize, y))

{- fall
	Checks if a piece can be moved down and if it can't returns a new piece and the old piece applied to the grid otherwise returns a new offset where the piece has been moved 1 step
-}
fall :: Grid -> Grid -> Position -> (Grid, Grid, Position)
fall board piece (x, y) = if (validPlace board piece (x,y+1))
		then (board, piece, (x, y+1))
		else (newBoard, newPiece, (5, 0))
	where
		canFall = overlap board piece (x, y+1)
		newPiece = randomPiece;
		newBoard = mergeGrids board piece (x, y)

{- mergeGrids grid1 grid2 offset
	Merges two grids into a single grid with the size of grid1
-}
mergeGrids :: Grid -> Grid -> Position -> Grid
mergeGrids [] _ _ = []
mergeGrids grid1 [] _ = grid1
mergeGrids (g:gs) (h:hs) (x,y)
		| y > 0 = g:(mergeGrids (gs) (h:hs) (x,(y-1)))
		| otherwise = (mergeRows g h (x,y)):(mergeGrids (gs) (hs) (x,y))
		where
			mergeRows [] _ _ = []
			mergeRows row1 [] _ = row1
			mergeRows (g:gs) (h:hs) (x,y)
				| x > 0 = g:(mergeRows gs (h:hs) (x-1,y))
				| isVoid h = g:(mergeRows (gs) (hs) (x,y))
				| otherwise = h:(mergeRows (gs) (hs) (x,y))

validPlace :: Grid -> Grid -> Position -> Bool
validPlace board piece offset = (not (overlap board piece offset)) && (inBounds piece offset boardBounds)

inBounds :: Grid -> Position -> Position -> Bool
inBounds [] _ _ = True
inBounds (r: rs) (x,y) bounds
	| inBoundsRow r (x,y) bounds = inBounds rs (x,y+1) bounds
	| otherwise = False
	where
		inBoundsRow :: [Block] -> Position -> Position -> Bool
		inBoundsRow [] _ _ = True
		inBoundsRow (Block _:bs) (x, y) bounds@(bX, bY)
			| x < 0 = False
			| x>=bX = False
			| y < 0 = False
			| y>=bY = False
			| otherwise = inBoundsRow bs (x+1, y) bounds
		inBoundsRow (_:bs) (x, y) bounds = inBoundsRow bs (x+1, y) bounds

{- randomPiece
	Returns a random piece
-}
randomPiece :: Grid
randomPiece =  shapes !! 4


{- applyMove
	Checks if a piece can be moved the given movement and returns a new offset
-}
applyMove :: Grid -> Grid -> Position -> Float -> Position
applyMove board piece offset@(x,y) movement = if cM
		then (x+movement,y)
		else offset
	where
		cM = canMove board piece offset movement

{- canMove
	Checks if a piece can be moved the indicated movement
-}
canMove :: Grid -> Grid -> Position -> Float -> Bool
canMove board piece (x,y) movement = not (overlap board piece o)
	where
		o = (x+movement, y)


{- applyRotate
	Checks if you can rotate and if you can return the rotated piece
-}
applyRotate :: Grid -> Grid -> Position -> Grid
applyRotate board piece offset =
	if (overlap board newPiece offset)
		then piece
		else newPiece
	where
		newPiece = turn piece



{- linesCleared board
	Returns a board where if any lines are full they are cleared and gives how many lines were cleared
-}
linesCleared :: Grid -> Int -> (Grid, Int)
linesCleared [] 0 = ([],0)
linesCleared board@(x:xs) n
		| lineFull x = (clear' x):(linesCleared xs (n+1))
		| not(lineFull x) = x:(linesCleared xs n)
		| otherwise = (board,n)
			where 
				clear' [] = []
				clear' [Block _] = [Void]
				clear' [Void] = [Void]
				clear' (x:xs) = (clear' x):(clear' xs)

 -- Tests a simple rotation of a small grid
test1 = TestCase $ assertEqual "rotate"
	([ [ Void, Block red ], [Void, Block red] ]) (turn [ [ Block red, Block red ], [Void, Void] ])
-- Tests if a simple move is allowed or not. True if allowed.
test2 = TestCase $ assertEqual "canMove1"
	 (True) (canMove  ([[Void,Void,Void],[Void,Void,Void]]) ([[Void,Void], [Block blue, Block blue]]) (0,0) (1))
-- -""-
test3 = TestCase $ assertEqual "canMove2"
	(False) (canMove ([[Void,Void,Void],[Void,Block blue, Block blue]]) ([[Void,Void],[Block blue, Block blue]]) (0,0) (1))

test4 = TestCase $ assertEqual "overlap1"
	(True) (overlap ([[Void, Void],  [Block blue, Block blue]])  ([[Void, Block blue],  [Void, Void]]) (0,1) )

test5 = TestCase $ assertEqual "overlap2"
	(False) (overlap ([[Void, Void],  [Block blue, Block blue]])  ([[Void, Block blue],  [Void, Void]]) (0,0) )

test6 = TestCase $ assertEqual "overlap3"
	(True) (overlap ([[Void, Void],  [Void, Block blue]])  ([[Block blue, Void],  [Void, Void]]) (1,1) )

runtests = runTestTT $ TestList [test1,test2,test3,test4, test5, test6]
