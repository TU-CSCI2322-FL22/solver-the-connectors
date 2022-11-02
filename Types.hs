module Board where
import Data.List

------------------------------------------MILESTONE ONE-----------------------------------------------
{-For this milestone, you will need to be able to represent the board game in Haskell, make moves on 
the board game, and tell if a player has won the board game. -}

--Written by MTP below

data Board = Board [Column] Color deriving (Show, Eq) --[Columns] and Color of player currently making move
--create an instance of Show for Board to show the current game state
data Color = Red | Black | Neither deriving (Show, Eq)
showcolor (Red) = "0"
showColor (Black) = "X"
type Column = [Color]
type Move = Int

rows = 6
columns = 7

-- Used to keep track of what level we are making our move on; SC
--colCounter = [ [(x,1)] | x <- [1..columns] ] 

initialBoard = Board [[] | x <- [1..columns]] Red -- Board [[],[],[],[],[],[],[]] Red

availableMoves :: Board -> [Move] -> Int -> [Move] -- Board -> [initially empty move lst] -> (index count) -> [resulting move lst]
availableMoves (Board [] clr) lst cnt = lst
availableMoves (Board (c:cs) clr) lst cnt =  
    if (length c < 6) then (availableMoves (Board cs clr) (cnt:lst) (cnt+1)) 
    else (availableMoves (Board cs clr) (lst) (cnt+1))

updateBoard :: Board -> Move -> Maybe Board
updateBoard (Board (x:xs) clr) col = undefined --if () then Nothing else Just ...

winnerRow :: Board -> (Color, Bool)
winnerRow = undefined


winnerColumn :: Board -> (Color, Bool)
winnerColumn (Board [] colr) = (Neither, False) 
winnerColumn (Board (b:bs) colr) = 
    let helperresult = aux b;
    in if (snd (helperresult) == True) then helperresult else winnerColumn (Board bs colr)
    where aux lst = 
                let grouped = group lst
                    fourcolor = [head x | x <- grouped, (length x) >= 4]
                in if fourcolor == [] then (Neither, False) else (head fourcolor, True)

winnerDiagonalAsc :: Board -> (Color, Bool)
winnerDiagonalAsc = undefined 

winnerDiagonalDes :: Board -> (Color, Bool)
winnerDiagonalDes = undefined 

isWinner :: Board -> Maybe Color --returns the Color and True if there is a a winner, so that we know which player won. 
isWinner bd = undefined --snd (winnerColumn bd) | snd (winnerRow bd) | snd (winnerDiagonalAsc bd) | snd (winnerDiagonalDes bd)
--Data type for outcome to check if tie
--
--Written by MTP above

--We will additonally need "A pretty show function for a game state, to ease debugging."
--Full Credit: All of these functions should consider possible errors or edge cases: what if there no winner, what if the move is not legal for the current game, etc. Use Maybe's or Either's appropriately.--

--TESTER CODE--
testWC = winnerColumn (Board [[Red, Black, Red, Black, Black, Red], [Red, Black, Black, Black, Black, Red], [Black, Red, Black, Red, Red, Black]] Red) --WORKS
testAM = availableMoves (Board [[Red, Black, Red, Black, Red, Black], [Red, Black, Black, Black, Red], [Black, Red, Black]] Red) [] 1 --WORKS