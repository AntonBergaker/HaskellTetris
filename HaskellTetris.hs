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
		allPictures = pictures (boardPictures ++ piecePictures ++ scorePictures ++ borderPictures ++ levelPictures ++ previewPictures)
		levelPictures   = renderLevel score
		borderPictures  = renderBorder
		scorePictures   = renderHighscore score
		boardPictures   = renderGrid board (0,0) 1
		piecePictures   = renderGrid piece (x*blockSize, y*blockSize) 1
		previewPictures = renderPreview piece (prevX*blockSize, prevY*blockSize)
		(prevX, prevY)  = dive board piece (x,y)

renderBorder :: [Picture]
renderBorder = 
				[color boardColor (line [(-250,-320), (-250,320)])] ++
				[color boardColor (line [(  70,-320), (  70,320)])] ++
				[color boardColor (line [(-250,-321), ( 70,-321)])]
			   

renderHighscore :: Int -> [Picture]
renderHighscore score =
	[translate 120 55 $ scale 0.2 0.2 $ color boardColor $ text (show score)] ++
	[translate 120 85 $ scale 0.2 0.2 $ color boardColor $ text ("SCORE")]

{- renderLevel score
	Returns a list of pictures showing the level at a position with the caption "LEVEL" based on the supplied score
	RETURNS: Two pictures in a list showing the level at a position based on the supplied score
-}
renderLevel :: Int -> [Picture]
renderLevel score = 
	[translate 120 125 $ scale 0.2 0.2 $ color boardColor $ text (show (getLevel score))] ++
	[translate 120 155 $ scale 0.2 0.2 $ color boardColor $ text ("LEVEL")]


{- renderGrid grid position alpha
	Returns a list of pictures from the given row and its position on screen with the given alpha value
	RETURNS: A list of pictures based on the supplied grid and position with the given alpha value
-}
renderGrid :: Grid -> Position -> Float -> [Picture]
renderGrid [] _ _ = []
renderGrid (r:rs) p@(x,y) alpha = (renderRow r p alpha) ++ (renderGrid rs (x,y+blockSize) alpha)

{- renderRow row position alpha
	Returns a list of pictures from the given row and its position on screen with the given alpha value
	RETURNS: A list of pictures based on the supplied row and position with the given alpha value
-}
renderRow :: [Block] -> Position -> Float -> [Picture]
renderRow [] _  _ = []
renderRow  (Void:bs)     (x,y) alpha = renderRow bs (x+blockSize, y) alpha
renderRow ((Block c):bs) (x,y) alpha = (color ( withAlpha alpha c) $ translate (x-234) (304-y) $ rectangleSolid blockSize blockSize) : (renderRow bs (x+blockSize, y) alpha)

{- renderPreview piece position 
	Returns a list of pictures representing the faded grid at the given position
	RETURNS: A collection of pictures based on the supplied grid and position
-}
renderPreview :: Grid -> Position -> [Picture]
renderPreview grid position = renderGrid grid position 0.5


{- checkGameOver board
	Checks whether the current game is lost
	RETURNS: True if there's a piece on the top of the board
-}
checkGameOver :: Grid -> Bool
checkGameOver board = not (lineEmpty (head board))

{- getLevel score
	Returns the level based on the current score where the level is 1-9
	RETURNS: The level based on the current score where the level is 1-9
	EXAMPLE: getLevel 1251 = 3
-}
getLevel :: Int -> Int
getLevel score 
		| score >= 4500 = 9
		| score <= 0 = 1
		| otherwise = (score+500) `div` 500


{- fall board piece offset time score
	Updates the game by moving the current piece down one step. This can mean you lose the game, that you clear rows and get points and get new pieces
	RETURNS: An updated board, piece and position and score that has been updated according to the game rules
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


{- dive board piece offset
	Returns the bottom level position at a specific offset based on the grid and piece
	RETURNS: The lowest point the given piece can reach in a straight line without colliding or breaching the bounds
-}
dive :: Grid -> Grid -> Position -> Position
dive board piece (x,y)
		| validPlace board piece (x,y+1) = dive board piece (x,(y+1))
		| otherwise = (x,y)


{- getScore clearedLines
	Returns how much score to get based on how many lines was cleared
	RETURNS: How many points a certain number of cleared lines was worth
	EXAMPLE: getScore 3 = 320
-}
getScore :: Int -> Int
getScore score = if (score <= 0)
	then 0
	else (score^2)*30+50


{- mergeGrids board piece offset
	Merges two grids into a single grid with the size of grid1 at the given offset
	PRE: The y value of offset must be equal or larger than 0
	RETURNS: A grid where both grids have been merged into one at the given offset
	EXAMPLE: mergeGrids [[Block blue, Void],[Void, Void]] [[Block blue, Block red], [Void, Void]] (0,1) = [[Block blue, Void], [Block blue, Block red]]
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


{- validPlace board piece offsets
	Checks if a given piece and offset does not collide with the board and that it's inside the bounds
	RETURNS: True if the given place and offset does not collide and is inside the bounds
-}
validPlace :: Grid -> Grid -> Position -> Bool
validPlace board piece offset = (not (overlap board piece offset)) && (inBounds piece offset boardBounds)

{- inBounds piece offset bounds
	Checks if a given piece is inside the games boundries as supplied with the bounds argument
	RETURNS: True if the piece is inside the bounds, False if it's partially or completely outside
	INVARIANT: Size of the grid
-}
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

{- randomPiece time
	Returns a "random" tetris piece based on the current gametime
	RETURNS: One of the seven tetris shapes semi-randomly based on the time
	EXAMPLE: randomPiece 1.012313 = [[Block (RGBA 0.6784314 0.38039216 0.92941177 1.0),Block (RGBA 0.6784314 0.38039216 0.92941177 1.0)],[Block (RGBA 0.6784314 0.38039216 0.92941177 1.0),Block (RGBA 0.6784314 0.38039216 0.92941177 1.0)]]
-}
randomPiece :: Float -> Grid
randomPiece time =  shapes !! ( (round (time*100000000000000000)) `mod` 7)


{- applyMove board piece offset movement
	Checks if you can move a piece to a given location, and if you can return that location
	RETURNS: The new location if the movement was allowed. The old location if not
-}
applyMove :: Grid -> Grid -> Position -> Float -> Position
applyMove board piece offset@(x,y) movement = if cM
		then (x+movement,y)
		else offset
	where
		cM = canMove board piece offset movement

{- canMove board piece offset movement
	Checks if a piece can be moved left or right based on the supplied movement and offset
	RETURNS: True if it can be moved, false if not
-}
canMove :: Grid -> Grid -> Position -> Float -> Bool
canMove board piece (x,y) movement = (validPlace board piece o)
	where
		o = (x+movement, y)


{- applyRotate board piece offset
	Checks if you can rotate the piece at the given position and if you can returns the rotated piece
	RETURNS: The rotated grid if it was able to rotate, the supplied grid if it was not able to rotate
-}
applyRotate :: Grid -> Grid -> Position -> Grid
applyRotate board piece offset =
	if (validPlace board newPiece offset)
		then newPiece
		else piece
	where
		newPiece = turn piece



{- clearLines board
	Returns a board where if any lines are full they are cleared and returns the new grid with the lines removed as well as how many was cleared
	RETURNS: (Board where the filled lines are cleared, Int of how many lines were cleared)
	INVARIANT: Size of the grid
	EXAMPLE: clearLines [[Block blue, Void] , [Block blue, Block blue]] = ([[Void, Void], [Block blue, Void]], 1)
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
 -- Tests a more complex move which is not allowed-
test3 = TestCase $ assertEqual "canMove2"
	(False) (canMove ([[Void,Void,Void],[Void,Block blue, Block blue]]) ([[Void,Void],[Block blue, Block blue]]) (0,0) (1))
 -- Tests an overlap with an offset
test4 = TestCase $ assertEqual "overlap1"
	(True) (overlap ([[Void, Void],  [Block blue, Block blue]])  ([[Void, Block blue],  [Void, Void]]) (0,1) )
 -- Tests an overlap with an offset
test5 = TestCase $ assertEqual "overlap2"
	(False) (overlap ([[Void, Void],  [Block blue, Block blue]])  ([[Void, Block blue],  [Void, Void]]) (0,0) )
 -- Tests a more complex overlap with an offset
test6 = TestCase $ assertEqual "overlap3"
	(True) (overlap ([[Void, Void],  [Void, Block blue]])  ([[Block blue, Void],  [Void, Void]]) (1,1) )
 -- Tests clearlines where lines can be cleared
test7 = TestCase $ assertEqual "clearLines1"
	([[Void, Void], [Block blue, Void]], 1) (clearLines [[Block blue, Void] , [Block blue, Block blue]] )
 -- Tests clearlines where no lines can be cleared
test8 = TestCase $ assertEqual "clearLines2"
	([[Block blue, Void] , [Void, Block blue]], 0) (clearLines [[Block blue, Void] , [Void, Block blue]] )
 -- Tests if two grids are successfully merged
test9 = TestCase $ assertEqual "mergeGrids"
 	([[Block blue, Void], [Block blue, Block red]]) (mergeGrids [[Block blue, Void],[Void, Void]] [[Block blue, Block red], [Void, Void]] (0,1))
-- Runs all tests
runtests = runTestTT $ TestList [test1,test2,test3,test4, test5, test6, test7, test8, test9]
