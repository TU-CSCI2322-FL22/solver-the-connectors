module Board where

data Board = Board [Column] Color deriving (Eq, Show) 
--Each Board data type has a list of columns and a color, the color indicates whose
--turn is next
data Color = Red | Black | None deriving (Eq, Show)
--None represents neither color; Using it for state
type Column = (Int, [Color])
--The Int represents the column numbers, and then the list of colors in that column
--with the item at index zero in [Color] being the piece at the "bottom"
--To find the row number, we'd use the piece's index in [Color]
type Piece = ((Int,Int), Color)
--This represents a piece, with it's coordinates (row, column) and color
type Move = Int
--which column the move is going into
type State = (Board, Color)
--state of the board, where the color represents the winner if there is one 
-- A little superfluous but we'll see.

validColumns :: Board -> [Int]
validColumns = undefined

--validMove checks if a move can be made
validMove :: Move -> Board -> Bool
validMove = undefined

--makeMove accepts a move and returns the Board after the move has been made, and
--whether or not there has been a winner
makeMove :: Move -> State
makeMove = undefined




