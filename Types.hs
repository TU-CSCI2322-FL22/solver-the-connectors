module Board where
import Data.List

------------------------------------------MILESTONE ONE-----------------------------------------------
{-For this milestone, you will need to be able to represent the board game in Haskell, make moves on 
the board game, and tell if a player has won the board game. -}

--Written by MTP below

data Board = Board [Column] Color deriving (Eq, Show) 
--Each Board data type has a list of columns and a color, the color indicates whose
--turn is next
--[Columns] and Color of player currently making move
--create an instance of Show for Board to show the current game state
data Color = Red | Black | Neither deriving (Eq, Show)
--None represents neither color; Using it for state
type Column = [Color]
--list of colors in that column
--with the item at index zero in [Color] being the piece at the "bottom"
--To find the row number, we'd use the piece's index in [Color]
type Piece = ((Int,Int), Color)
--This represents a piece, with it's coordinates (row, column) and color
type Move = Int
--which column the move is going into
type State = (Board, Color)
--state of the board, where the color represents the winner if there is one 
-- A little superfluous but we'll see.
type Coordinat3e = (Int, Int)

showcolor (Red) = "0"
showColor (Black) = "X"

rows = 6
columns = 7


-- Used to keep track of what level we are making our move on; SC
--colCounter = [ [(x,1)] | x <- [1..columns] ] 

initialBoard = Board [[] | x <- [1..columns]] Red 
-- Board [[],[],[],[],[],[],[]] Red


availableMoves :: Board -> [Move] 
--Changed it wso availableMoves only needs the Board as an argument to return available moves
-- Board -> [initially empty move lst] -> (index count) -> [resulting move lst]
availableMoves brd = 
    aux brd [] 0
    where
        aux :: Board -> [Move] -> Int -> [Move]
        aux (Board [] clr) lst cnt = lst
        aux (Board (c:cs) clr) lst cnt =  
            if length c < rows then aux (Board cs clr) (cnt:lst) (cnt+1) 
            else aux (Board cs clr) lst (cnt+1)
            --also switched out 6 for rows to make it more abstract (?)

--was pattern matched but we tweakin
--make sure when you call updateBoard passing in a 0 for count initially 
updateBoard :: Board -> Move -> Int -> [Column] -> Board
updateBoard (Board (x:xs) clr) mv cnt accumlst = 
    if (cnt == mv) then Board ((accumlst ++ (x:clr) ++ xs) switchColor(clr)) 
    else updateBoard (Board xs switchColor(clr)) mv (cnt+1) (x:accumlst)

{-updateBoard b@(Board (x:xs) clr) mv = 
    if mv `elem` (availableMoves b)
    then
         aux (Board (x:xs) clr) mv mv = 
         aux (Board (x:xs) clr) cnt mv = 
        if mv > cnt then aux (Board (x:xs) clr) (cnt + 1) mv 
        else -}
        
    
switchColor :: Color -> Color
switchColor clr
    | clr == Black = Red
    | clr == Red = Black
--if clr == Black then clr = Red else clr = Black
--if () then Nothing else Just ...

{-makeMove :: Board -> Move -> Board
makeMove brd mv = 
    if mv `elem` availableMoves(brd) 
        then updateBoard brd mv
    else -}
--if move `elem` availableMoves brd then else return error
--If we check if it's a valid move before, do we need a Maybe Board? TBH I still don't really understand
--Maybe. I think it would elimate the need for updateBoard, but they're kind of going to do the same thing
{-winnerRow :: Board -> (Color, Bool)
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
winnerDiagonalDes = undefined -}

getColumn :: Board -> Move -> Row
getColumn (Board cols clr) mv = 
        let
            drC = drop (m - 1) cols
        in 
            if drC == [] then [] else head drC

getCoordinate :: Board -> Move -> (Int, Int)
getCoordinate (Board col clr) mv = (last getColumn ((Board cols clr) mv) , mv) --(Int, Int)

--If no color is in that position, or if that position is out of bounds, it will return []
findColor :: Board -> (Int, Int) -> Color
findColor (Board bd clr) (x,y) = 
    let 
        drC = drop (y - 1) bd
        col = if drC == [] then [] else head drC
        beforein = if col == [] then Neither else drop (x - 1) col
    in 
        if beforein == Neither 
        then Neither 
        else (if drop(x-1) col == [] then Neither else head (drop (x - 1) col))


leftAsc :: Board -> Color -> (Int, Int) -> Int -> Int --Board -> Color -> Move -> Count ->4 FinalCount
leftAsc (Board cols clr) mv 4 = 4
leftAsc (Board cols clr) mv cnt = undefined

rightAsc :: Board -> Color -> (Int, Int) -> Int -> Int
rightAsc (Board cols clr) mv 4 = 4
rightAsc (Board cols clr) mv cnt = undefined

leftDsc :: Board -> Color -> (Int, Int) -> Int -> Int --Board -> Color -> Move -> Count ->4 FinalCount
leftDsc (Board cols clr) mv 4 = 4
leftDsc (Board cols clr) mv cnt = undefined

rightDsc :: Board -> Color -> (Int, Int) -> Int -> Int
rightDsc (Board cols clr) c (row, col) 4 = 4
rightDsc (Board cols clr) c (row, col) cnt = 
    let
        nextPosition = (row - 1, col + 1)
        newCol = findColor (Board cols clr) nextPosition
    in
        if newCol == c
        then 1 + rightDsc (Board cols clr) c nextPosition cnt 

down :: Board -> Color -> (Int, Int) -> Int -> Int
down (Board cols clr) mv 4 = 4 
down (Board cols clr) mv cnt = 

right :: Board -> Color -> (Int, Int) -> Int -> Int
right (Board cols clr) mv 4 = 4 
right (Board cols clr) mv cnt = 

left :: Board -> Color -> (Int, Int) -> Int -> Int
left (Board cols clr) mv 4 = 4 
left (Board cols clr) mv cnt = 




isWinner :: Board -> Move -> Maybe Color --returns the Color and True if there is a a winner, so that we know which player won. 
isWinner (Board cols clr) mv =  
    let newcolor = --snd (winnerColumn bd) | snd (winnerRow bd) | snd (winnerDiagonalAsc bd) | snd (winnerDiagonalDes bd)
--Data type for outcome to check if tie
--
--Written by MTP above

--We will additonally need "A pretty show function for a game state, to ease debugging."
--Full Credit: All of these functions should consider possible errors or edge cases: what if there no winner, what if the move is not legal for the current game, etc. Use Maybe's or Either's appropriately.--

--TESTER CODE--
--testWC = winnerColumn (Board [[Red, Black, Red, Black, Black, Red], [Red, Black, Black, Black, Black, Red], [Black, Red, Black, Red, Red, Black]] Red) --WORKS
testAM = availableMoves (Board [[Red, Black, Red, Black, Red, Black], [Red, Black, Black, Black, Red], [Black, Red, Black]] Red) --WORKS