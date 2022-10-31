module Board where
import Data.List

--Written by MTP below

data Board = Board [Column] Heights deriving (Show, Eq) --[Columns] and list of lengths of the columns
data Color = Red | Black | Empty deriving (Show, Eq )
type Heights = [(Int, Int)]
type Column = [Spot]
type Spot = (Coordinate, Color)
type Coordinate = (Int, Int) -- (row, column)

rows = 6
columns = 7

InitialBoard = Board [[] | x <- [1..columns]] [(x,0) | x <- [1..rows]]-- Board [[],[],[],[],[],[],[]] [(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0)]

availableMoves :: Board -> [Int]
availableMoves Board b h = [idx | (idx, cnt) <- h, cnt < row]

addMove :: Board -> Int -> Board
addMove (Board bd h) col = 
    let hghts [if (idx == col) then (idx, cnt+1) else (idx, cnt)| (idx,cnt) <- h]
        bord = --[if(col == idx) then ]
    in Board bord hghts


--Written by MTP above

