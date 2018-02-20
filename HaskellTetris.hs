import TetrisTypes
import Utils
import Graphics.Gloss
import Test.HUnit
import Pieces

main :: IO ()
main = display window background (render newGameState)

window :: Display
window = InWindow "Tetris" (400, 640) (10, 10)

background :: Color
background = black

blockSize :: Float
blockSize = 32

newGameState :: GameState
newGameState = (emptyBoard, t, (5,0), 0, 0)

emptyBoard :: Grid
emptyBoard = replicate 20 $ replicate 10 Void

{- render gamestate
	Creates a picture from the given gamestate
-}
render :: GameState -> Picture
render (board, piece, offset, score, time) = pictures (boardPictures ++ piecePictures)
	where
		boardPictures = renderGrid board (0,0)
		piecePictures = renderGrid piece offset

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
renderRow (Void:bs) (x,y) = renderRow bs (x+blockSize, y)
renderRow (Block c:bs) (x,y) = color c ((translate (x) (y) (rectangleSolid blockSize blockSize))) : (renderRow bs (x+blockSize, y))

{- fall
	Checks if a piece can be moved down and if it can't returns a new piece and the old piece applied to the grid otherwise returns a new offset where the piece has been moved 1 step
-}
fall :: Grid -> Grid -> Position -> (Grid, Grid, Position)
fall board piece (x, y) = if canFall
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
mergeGrids = undefined;


{- randomPiece
	Returns a random piece
-}
randomPiece :: Grid
randomPiece = undefined;

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
	([ [ Void, Block red ], [Void, Block red] ]) (turn [ [ Block red, Block red ], [Void, Void] ])
-- Tests if a simple move is allowed or not. True if allowed.
test2 = TestCase $ assertEqual "canMove"
	 (True) (canMove ([[Void,Void], [Block blue, Block blue]]) ([[Void,Void,Void],[Void,Void,Void]]) (0,0) (1))
-- -""-
test3 = TestCase $ assertEqual "canMove"
	(False) (canMove ([[Void,Void],[Block blue, Block blue]]) ([[Void,Void,Void],[Void,Block blue, Block blue]]) (0,0) (1))
runtests = runTestTT $ TestList [test1,test2,test3]
