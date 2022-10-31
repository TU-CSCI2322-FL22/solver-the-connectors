module Board where
import Data.List

--Written by MTP below

data Board = Board [Column] Heights deriving (Show, Eq) --[Columns] and list of lengths of the columns
data Color = Red | Black | Empty deriving (Show, Eq )
type Heights = [Int]
type Column = [Spot]
type Spot = (Coordinate, Color)
type Coordinate = (Int, Int) -- (row, column)

rows = 6
columns = 7

InitialBoard = Board [[] | x <- [1..columns]] [0 | x <- [1..rows]]-- Board [[],[],[],[],[],[],[]] [0,0,0,0,0,0,0]



--Written by MTP above

