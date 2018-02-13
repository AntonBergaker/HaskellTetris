
import Graphics.Gloss


data Block = Void | Block Color
type Grid = [[Block]]
type Position = (Int,Int)


type GameState = (Grid, Grid, Position, Int, Double)
