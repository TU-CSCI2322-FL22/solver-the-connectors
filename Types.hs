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
type Move = Int
--which column the move is going into
type State = (Board, Color)
--state of the board, where the color represents the winner if there is one 
-- A little superfluous but we'll see.
data Winner = YesWinner Color | NoWinner | Tie deriving (Eq, Show)
type Coordinate = (Int, Int)
type Direction = (Int, Int)



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
            if length c < rows 
            then aux (Board cs clr) (cnt:lst) (cnt+1) 
            else aux (Board cs clr) lst (cnt+1)
            --also switched out 6 for rows to make it more abstract (?)

--was pattern matched but we tweakin
--make sure when you call updateBoard passing in a 0 for count initially
--I feel like to updateBoard/makeMove we onlY need Board and Move as inputss 
updateBoard :: Board -> Move  -> Board
updateBoard (Board (x:xs) clr) mv  = 
    aux (Board (x:xs) clr) mv 0 []
    where 
        aux :: Board -> Move -> Int -> [Column] -> Board
        aux (Board (x:xs) clr) mv cnt accumlst = 
            let newcolor = if clr == Red then Black else Red
            in if (cnt == mv) 
               then (Board (accumlst ++ (clr:x):xs) newcolor)
               else aux (Board xs clr) mv (cnt+1) (x:accumlst)


makeMove :: Board -> Move -> (Board, Winner)
makeMove (Board (x:xs) clr) mv =
    let
        newBoard = updateBoard (Board (x:xs) clr) mv
        win = checkWinner newBoard clr mvCoordinate
        mvCoordinate = (length (getColumn (Board (x:xs) clr) mv) ,mv)
    in (newBoard, win)

ub = updateBoard (Board [[Red, Black, Red, Black, Red, Black], [Red, Black, Black, Black, Red], [Black, Red, Black]] Red) 1 
{-updateBoard b@(Board (x:xs) clr) mv =
    if mv `elem` (availableMoves b)
    then
         aux (Board (x:xs) clr) mv mv = 
         aux (Board (x:xs) clr) cnt mv = 
        if mv > cnt then aux (Board (x:xs) clr) (cnt + 1) mv 
        else -}
        
    
{-switchColor :: Color -> Color
switchColor clr
    | clr == Black = Red
    | clr == Red = Black-}
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


--Takes a board and a move (column number) and returns the column
--returns [] if the column is empty or out of bounds
--otherwise it returns a column

getColumn :: Board -> Move -> Column
getColumn (Board cols clr) mv = 
        let
            drC = drop (mv - 1) cols
        in 
            if drC == [] then [] else head drC

--findColor takes a Board and a Coordinate and finds the Color at that coordinate
--If there is no color in the coordinate, or if that position is out of bounds, it will return neither
--NOTE: Our makeMove function must prevent out of bound moves
--(This doesn't check if it's out of bounds, it's just saying that if their is not a color in that
--position it must be either empty or out of bounds)
findColor :: Board -> Coordinate -> Color
findColor (Board cols clr) (x,y) = 
    let
        col = getColumn (Board cols clr) y
    in
        if col == []
        then error "Not a valid coordinate! No column! (Either move hasn't been made here or out of bounds)"
        else getColorAtRow col x
    where
        getColorAtRow :: Column -> Int -> Color
        getColorAtRow cl r
            |drop (r-1) cl == [] = error "Not a valid coordinate! No row! (Either move hasn't been made here or out of bounds)"
            |otherwise = head (drop (r-1) cl)

    
--takes board, Color we're checking for, coord of piece we're "at", direction we're going in, and a count
--returns a count of how many in a row of the color were checking there is in that Direction
countDir :: Board -> Color -> Coordinate -> Direction -> Int -> Int
countDir (Board cols cl) cChecking (row, col) (mvR, mvC) cnt =
    let
        nextPos = (row + mvR, col + mvC)
        nextPosCol = findColor (Board cols cl) nextPos
    in
        if nextPosCol == cChecking
        then countDir (Board cols cl) cChecking nextPos (mvR, mvC) (cnt + 1)
        else cnt


checkWinner :: Board -> Color -> Coordinate -> Winner
checkWinner (Board cols cl) cChecking (row, col) =
    let
        --I don't like having all this repeated but I didn't want to deal with an
        --association list when trying to add the directions together
        mvsAvl = availableMoves (Board cols cl)--if this equals zero and there is no winner, they tie for losers
        
        lftAsc = countDir (Board cols cl) cChecking (row, col) (1,-1) 0
        rtAsc = countDir (Board cols cl) cChecking (row, col) (1,1) 0
        lft = countDir (Board cols cl) cChecking (row, col) (0,-1) 0
        rt = countDir (Board cols cl) cChecking (row, col) (0,1) 0
        lftDsc = countDir (Board cols cl) cChecking (row, col) (-1,-1) 0
        dw = countDir (Board cols cl) cChecking (row, col) (-1, 0) 0
        rtDsc = countDir (Board cols cl) cChecking (row, col) (-1,1) 0

        fstDiag = lftAsc + 1 + rtDsc
        sndDiag = rtAsc + 1 + lftDsc
        horz = lft + 1 + rt
        vert = dw + 1
    in  
        if fstDiag > 3 || sndDiag > 3 || horz > 3 || vert > 3
        then YesWinner cChecking --winner
        else 
            if null mvsAvl
            then Tie
            else NoWinner

--We will additonally need "A pretty show function for a game state, to ease debugging."
--Full Credit: All of these functions should consider possible errors or edge cases: what if there no winner, what if the move is not legal for the current game, etc. Use Maybe's or Either's appropriately.--

--TESTER CODE--
--testWC = winnerColumn (Board [[Red, Black, Red, Black, Black, Red], [Red, Black, Black, Black, Black, Red], [Black, Red, Black, Red, Red, Black]] Red) --WORKS
testAM = availableMoves (Board [[Red, Black, Red, Black, Red, Black], [Red, Black, Black, Black, Red], [Black, Red, Black]] Red) --WORKS
