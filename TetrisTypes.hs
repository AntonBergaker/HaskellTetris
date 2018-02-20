module TetrisTypes( Block (Block, Void) , Grid, Position, GameState) where
import Graphics.Gloss


data Block = Void | Block Color
	deriving (Eq, Show)
type Grid = [[Block]]
type Position = (Float,Float)


type GameState = (Grid, Grid, Position, Int, Float)
