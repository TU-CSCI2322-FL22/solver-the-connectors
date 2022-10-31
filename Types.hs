module Board where
import Data.List

------------------------------------------MILESTONE ONE-----------------------------------------------

--Written by MTP below

data Board = Board [Column] Heights deriving (Show, Eq) --[Columns] and list of lengths of the columns
data Color = Red | Black deriving (Show, Eq )
type Heights = [(Int, Int)]
type Column = [Spot]
type Spot = (Coordinate, Color)
type Coordinate = (Int, Int) -- (row, column)

rows = 6
columns = 7

initialBoard = Board [[] | x <- [1..columns]] [(x,0) | x <- [1..rows]]-- Board [[],[],[],[],[],[],[]] [(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0)]

availableMoves :: Board -> [Int]
availableMoves Board b h = [idx | (idx, cnt) <- h, cnt < row]

addMove :: Board -> Int -> Board
addMove (Board bd h) col = 
    let hghts [if (idx == col) then (idx, cnt+1) else (idx, cnt)| (idx,cnt) <- h]
        bord = --[if(col == idx) then ]
    in Board bord hghts

winnerRow :: Board -> (Color, Bool)
winnerRow = undefined

winnerColumn :: Board -> (Color, Bool)
winnerColumn = undefined

winnerDiagonal :: Board -> (Color, Bool)
winnerDiagonal = undefined 

isWinner :: Board -> (Color, Bool) --returns the Color and True if there is a a winner, so that we know which player won. 
isWinner bd = snd (winnerColumn bd) | snd (winnerRow bd) | snd (winnerDiagonal bd)

--Written by MTP above

--We will additonally need "A pretty show function for a game state, to ease debugging."
--Full Credit: All of these functions should consider possible errors or edge cases: what if there no winner, what if the move is not legal for the current game, etc. Use Maybe's or Either's appropriately.