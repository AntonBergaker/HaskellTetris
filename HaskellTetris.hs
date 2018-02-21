import TetrisTypes
import Utils
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Test.HUnit
import Pieces
import System.Random

main :: IO ()
main = play window background 60 newGameState render handleKeys update

window :: Display
window = InWindow "Tetris" (500, 640) (10, 10)

background :: Color
background = black

boardColor :: Color
boardColor = makeColor (178/255) (240/255) (104/255) 1;

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
handleKeys (EventKey (SpecialKey KeyUp   ) Down _ _) (board, piece, offset, score, time) = (board, newPiece, offset, score, time)
	where
		newPiece = applyRotate board piece offset
handleKeys (EventKey (SpecialKey KeyDown ) Down _ _) (board, piece, offset, score, time) = (board , piece, newOffsetDive, score, time)
	where
		newOffsetDive = dive board piece offset
handleKeys _ gameState = gameState


update :: Float -> GameState -> GameState
update inc (board, piece, offset, score, time) =
		if (totalTime > fallTime)
			then (newBoard, newPiece, newOffset, newScore, totalTime - fallTime)
			else (board   , piece   , offset   , score   , totalTime)
	where
		totalTime = time + inc
		fallTime = 0.5 - ((fromIntegral level) / 25)
		level = getLevel score
		(newBoard, newPiece, newOffset, newScore) = fall board piece offset time score

{- render gamestate
	Creates a picture from the given gamestate
-}
render :: GameState -> Picture
render (board, piece, (x,y), score, time) = allPictures
	where
		allPictures = pictures (boardPictures ++ piecePictures ++ scorePictures ++ borderPictures ++ levelPictures)
		levelPictures  = renderLevel score
		borderPictures = renderBorder
		scorePictures  = renderHighscore score
		boardPictures  = renderGrid board (0,0)
		piecePictures  = renderGrid piece (x*blockSize, y*blockSize)

renderBorder :: [Picture]
renderBorder = 
				[color boardColor (line [(-250,-320), (-250,320)])] ++
				[color boardColor (line [(  70,-320), (  70,320)])] ++
				[color boardColor (line [(-250,-321), ( 70,-321)])]
			   

renderHighscore :: Int -> [Picture]
renderHighscore score =
	[translate 120 55 $ scale 0.2 0.2 $ color boardColor $ text (show score)] ++
	[translate 120 85 $ scale 0.2 0.2 $ color boardColor $ text ("SCORE")]

renderLevel :: Int -> [Picture]
renderLevel score = 
	[translate 120 125 $ scale 0.2 0.2 $ color boardColor $ text (show (getLevel score))] ++
	[translate 120 155 $ scale 0.2 0.2 $ color boardColor $ text ("LEVEL")]


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
renderRow ((Block c):bs) (x,y) = color c ((translate (x-234) (304-y) (rectangleSolid blockSize blockSize))) : (renderRow bs (x+blockSize, y))

checkGameOver :: Grid -> Bool
checkGameOver board = not (lineEmpty (head board))

{- getLevel score
	Returns the level based on the current score 1-9
-}
getLevel :: Int -> Int
getLevel score 
		| score >= 4500 = 9
		| score <= 0 = 1
		| otherwise = (score+500) `div` 500


{- fall
	Checks if a piece can be moved down and if it can't returns a new piece and the old piece applied to the grid otherwise returns a new offset where the piece has been moved 1 step
-}
fall :: Grid -> Grid -> Position -> Float -> Int -> (Grid, Grid, Position, Int)
fall board piece (x, y) time score
		| canFall == False && gameOver = (emptyBoard, newPiece, (5, 0), 0)
		| canFall = (board, piece, (x, y+1), score)
		| otherwise = (clearedBoard, newPiece, (5, 0), score + (getScore  newPoints))
	where
		(clearedBoard, newPoints) = clearLines newBoard
		gameOver = checkGameOver newBoard
		canFall = validPlace board piece (x,y+1)
		newPiece = randomPiece time;
		newBoard = mergeGrids board piece (x, y)


dive :: Grid -> Grid -> Position -> Position
dive board piece (x,y)
		| validPlace board piece (x,y+1) = dive board piece (x,(y+1))
		| otherwise = (x,y)


getScore :: Int -> Int
getScore score = if (score <= 0)
	then 0
	else (score^2)*30+50


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
				| x < 0 = (mergeRows (g:gs) (hs) (x+1,y))
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
randomPiece :: Float -> Grid
randomPiece time =  shapes !! ( (round (time*100000000000000000)) `mod` 7)


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
canMove board piece (x,y) movement = (validPlace board piece o)
	where
		o = (x+movement, y)


{- applyRotate
	Checks if you can rotate and if you can return the rotated piece
-}
applyRotate :: Grid -> Grid -> Position -> Grid
applyRotate board piece offset =
	if (validPlace board newPiece offset)
		then newPiece
		else piece
	where
		newPiece = turn piece



{- clearLines board
	Returns a board where if any lines are full they are cleared and gives how many lines were cleared
-}
clearLines :: Grid -> (Grid, Int)
clearLines board = ((replicate missingLines voidRow) ++ remainingLines, missingLines)
		where
			remainingLines = filter f board
			f row = not (lineFull row)
			missingLines = (length board) - (length remainingLines)
			voidRow = replicate (length (board !! 0)) Void




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

test7 = TestCase $ assertEqual "clearLines1"
	([[Void, Void], [Block blue, Void]], 1) (clearLines [[Block blue, Void] , [Block blue, Block blue]] )

test8 = TestCase $ assertEqual "clearLines2"
	([[Block blue, Void] , [Void, Block blue]], 0) (clearLines [[Block blue, Void] , [Void, Block blue]] )

runtests = runTestTT $ TestList [test1,test2,test3,test4, test5, test6, test7, test8]
