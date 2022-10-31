--Written by MTP below

data Board = Board [Column]
type Column = [Spot]
type Spot = (Coordinate, Color)
type Coordinate = (Int, Int) -- (row, column)
data Color = Red | Black | Empty deriving (Show, Eq, Ord)
rows = 6
columns = 7

InitialBoard = Board [((x,y), Empty) | x <- [1..rows], y <- [1..columns]]

--Written by MTP above

