module TetrisTypes( Block (Block, Void) , Grid, Position, GameState) where
import Graphics.Gloss

{- Block
  The data type represents a position in a grid.  It can either be a Void or contain
  a Block with a Color.
-}
data Block = Void | Block Color
    deriving (Eq, Show)
{- Grid
 Represents the board of the game. The type is a list that contains lists
 of Blocks, that is a 2D matrix.
-}
type Grid = [[Block]]
{- Position
  The type to represent the position on the grid. A tuple with two floats.
-}
type Position = (Float,Float)

{- GameState
  The type that contains all the game data. (board, piece, position, score, time)
-}
type GameState = (Grid, Grid, Position, Int, Float)
