module Board where
import Data.List

------------------------------------------MILESTONE ONE-----------------------------------------------
{-For this milestone, you will need to be able to represent the board game in Haskell, make moves on 
the board game, and tell if a player has won the board game. -}

--Written by MTP below

data Board = Board [Column] Color deriving (Show, Eq) --[Columns] and list of lengths of the columns
--create an instance of Show for Board to show the current game state
data Color = Red | Black deriving (Show, Eq)
showcolor (Red) = "0"
showColor (Black) = "X"
    
--type Heights = [(Int, Int)]
type Column = (Int,[Color])

--type Spot = (Coordinate, Color)
--type Coordinate = (Int, Int) -- (row, column)
type Move = Int

rows = 6
columns = 7

-- Used to keep track of what level we are making our move on; SC
--colCounter = [ [(x,1)] | x <- [1..columns] ] 

initialBoard = Board [[] | x <- [1..columns]] [(x,0) | x <- [1..rows]]-- Board [[],[],[],[],[],[],[]] [(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0)]

availableMoves :: Board -> [Int]
availableMoves Board b h = [idx | (idx, cnt) <- h, cnt < row]

updateBoard :: Board -> Int -> Int -> Board
updateBoard (Board (x:xs) hg) ct cl = if ct == cl then (,):x
addMove :: Board -> Int -> Maybe Board
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

isWinner :: Board -> Maybe Color --returns the Color and True if there is a a winner, so that we know which player won. 
isWinner bd = snd (winnerColumn bd) | snd (winnerRow bd) | snd (winnerDiagonal bd)
--Data type for outcome to check if tie
--
--Written by MTP above

--We will additonally need "A pretty show function for a game state, to ease debugging."
--Full Credit: All of these functions should consider possible errors or edge cases: what if there no winner, what if the move is not legal for the current game, etc. Use Maybe's or Either's appropriately.
