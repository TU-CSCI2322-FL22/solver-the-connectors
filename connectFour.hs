module Board where
import Data.List
import Data.Tuple.Extra (secondM)

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

initialBoard = Board [[] | x <- [1..columns]] Red 
-- Board [[],[],[],[],[],[],[]] Red
rows = 6
columns = 7 


showBoard :: Board -> [Char]
showBoard brd =
    aux brd rows
    where
        aux :: Board -> Int -> [Char]
        aux brd 0 = []
        aux brd cnt =
            let 
                mkCol = foldr (\x y -> if findColor brd (cnt, x) == Red
                                    then '0' :y 
                                    else if findColor brd (cnt, x) == Black 
                                         then 'X':y 
                                         else '-':y) [] [1..7] 
            in  
                mkCol ++ "\n" ++ aux brd (cnt-1)


--Makes a list of all columns where a move can be made 
availableMoves :: Board -> [Move]
availableMoves brd = 
    aux brd [] 1
    where
        aux :: Board -> [Move] -> Int -> [Move]
        aux (Board [] clr) lst cnt = lst
        aux (Board (c:cs) clr) lst cnt =  
            if length c < rows
            then aux (Board cs clr) (cnt:lst) (cnt+1) 
            else aux (Board cs clr) lst (cnt+1)

--updateBoard takes a Board and Move as arguments, and returns a new Board where that Move has been made
--and is represented in the Board. (Also returns the color of the person whose turn is next.)
--Note: we've already made sure that it's a valid move in makeMove
updateBoard :: Board -> Move  -> Board
updateBoard (Board (x:xs) clr) mv  = 
    aux (Board (x:xs) clr) mv 1 []
    where 
        aux :: Board -> Move -> Int -> [Column] -> Board
        aux (Board [] clr) mv cnt accumlst = Board accumlst (if clr == Red then Black else Red)
        aux (Board (x:xs) clr) mv cnt accumlst = 
            let newcolor = if clr == Red then Black else Red
            in if (cnt == mv) 
               then (Board (accumlst ++ (x ++ [clr]):xs) newcolor)
               else aux (Board xs clr) mv (cnt+1) (accumlst ++ [x])

--makeMove takes a Board and a Move and returns a tuple of the updated Board and the winner status
--If the move is not valid, there is an error
makeMove :: Board -> Move -> (Board, Winner)
makeMove (Board cols clr) mv =
    let
        avlMvs = availableMoves (Board cols clr) --makes lst of available moves
        newBoard = updateBoard (Board cols clr) mv -- creates new board that includes the new move piece
        win = checkWinner newBoard clr coordOfMv --checksWinner
        coordOfMv = (length (getColumn newBoard mv), mv) --coordinate of the move
    in 
        if mv `elem` avlMvs
        then (newBoard, win)
        else error "This is not a valid move! (makeMove)"

ub = updateBoard (Board [[Red, Black, Red, Black, Red, Black], [Red, Black, Black, Black, Red], [Black, Red, Black]] Red) 1 

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
        then Neither
        else if (x < 1 || y < 1) 
        then Neither
        else getColorAtRow col x
    where
        getColorAtRow :: Column -> Int -> Color
        getColorAtRow cl r
            |drop (r-1) cl == [] = Neither
            |otherwise = head (drop (r-1) cl)

    
--takes board, Color we're checking for, coord of piece we're "at", direction we're going in, and a count
--returns a count of how many in a row of the color were checking there is in that Direction
countDir :: Board -> Color -> Coordinate -> Direction -> Int -> Int
countDir (Board cols cl) cChecking (row, col) (mvR, mvC) 4 = 4 
countDir (Board cols cl) cChecking (row, col) (mvR, mvC) cnt =
    let
        nextPos = (row + mvR, col + mvC)
        nextPosCol = findColor (Board cols cl) nextPos
    in
        if nextPosCol == cChecking
        then countDir (Board cols cl) cChecking nextPos (mvR, mvC) (cnt + 1)
        else cnt

--cD = (Board [[Red, Black, Red, Black, Red, Red], [Red, Black, Black, Red, Black, Red], [Black, Red, Black, Red, Black, Red], [],[],[],[]] Black) Black ()
fC = countDir (Board [[Red, Red, Red, Black, Red, Red], [Red, Black, Black, Red, Black, Red], [Black, Red, Black, Red, Black, Red], [],[],[],[]] Black) Red (2,1) (-1,0) 0
findC = findColor (Board [[Red, Red, Red, Black, Red, Red], [Red, Black, Black, Red, Black, Red], [Black, Red, Black, Red, Black, Red], [],[],[],[]] Black) (-1,1)

checkWinner :: Board -> Color -> Coordinate -> Winner
checkWinner (Board cols cl) cChecking (row, col) =
    let 
        mvsAvl = availableMoves (Board cols cl)
        
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
        then YesWinner cChecking 
        else 
            if null mvsAvl
            then Tie
            else NoWinner

--Not sure if we need these:
showcolor (Red) = '0'
showColor (Black) = 'X'
showColor (Neither) = '-'


--Tester Code--

first = putStrLn (showBoard initialBoard)
sndM = makeMove initialBoard 4
sndBrd = putStrLn (showBoard (fst sndM))
thrdM = makeMove (fst sndM) 3
thrdBrd = putStrLn(showBoard (fst thrdM))
fM = makeMove (fst thrdM) 4
ffM = makeMove (fst fM) 3
sM = makeMove (fst ffM) 4
svM = makeMove(fst sM) 2
eM = makeMove (fst svM) 4
isThereWin = snd eM
nM = makeMove (fst eM) 4




--TESTER CODE--
sb = showBoard (Board [[Red, Black, Red, Black, Red, Black], [Red, Black, Black, Black, Red], [Black, Red, Black]] Red)
sBB = showBoard (Board [[Red, Black, Red, Black, Red, Red], [Red, Black, Black, Red, Black, Red], [Black, Red, Black, Red, Black, Red], [],[],[],[]] Black)
testWC = checkWinner (Board [[Red, Black, Red, Black, Red, Red], [Red, Black, Black, Red, Black, Red], [Black, Red, Black, Red, Black, Red], [],[],[],[]] Black) Red (2,1) 
testAM = availableMoves (Board [[Red, Black, Red, Black, Red, Black], [Red, Black, Black, Black, Red], [Black, Red, Black], [],[],[],[]] Red) 

testFC = findColor (Board [[Red, Black, Red, Black, Red, Red], [Red, Black, Black, Red, Black, Red], [Black, Red, Black, Red, Black, Red]] Black) (3,3)

test2 = checkWinner (Board [[Red, Black, Black, Red, Black, Red, Black], [Black, Red, Black, Red, Black, Red, Black], [Red, Black, Black, Red, Red], [Black,Red, Red, Red, Red, Black, Red], [Black],[Red, Black],[Black]] Black) Red (4,1)
b2 = putStrLn (showBoard (Board [[Red, Black, Black, Red, Black, Red, Black], [Black, Red, Black, Black, Black, Red, Black], [Red, Black, Black, Red, Red], [Black,Red, Red, Red, Red, Black, Red], [Black],[Red, Black],[Black]] Red))

test3 = checkWinner (Board [[Red, Black, Black, Red, Black, Red, Black], [Black, Black, Black, Black], [Red, Black, Black, Red, Red], [Black,Red, Red, Red, Black, Black, Red], [Black],[Red, Black],[Black]] Black) Black (4,2)
b3 = putStrLn (showBoard (Board [[Red, Black, Black, Red, Black, Red, Black], [Black, Black, Black, Black], [Red, Black, Black, Red, Red], [Black,Red, Red, Red, Black, Black, Red], [Black],[Red, Black],[Black]] Black))

test4 = checkWinner (Board [[Red, Black, Black, Red, Black, Red, Black], [Black, Red, Black, Black, Black, Red, Black], [Red, Black, Red, Red, Red], [Black,Red, Red, Red, Black, Black, Red], [Black],[Red, Black],[Black]] Black) Red (1,1)
b4 = putStrLn (showBoard (Board [[Red, Black, Black, Red, Black, Red, Black], [Black, Red, Black, Black, Black, Red, Black], [Red, Black, Red, Red, Red], [Black,Red, Red, Red, Black, Black, Red], [Black],[Red, Black],[Black]] Black))
b4u1 = putStrLn (showBoard (updateBoard (Board [[Red, Black, Black, Red, Black, Red, Black], [Black, Red, Black, Black, Black, Red, Black], [Red, Black, Red, Red, Red], [Black,Red, Red, Red, Black, Black, Red], [Black],[Red, Black],[Black]] Black) 5))
b4u2 =  putStrLn (showBoard(updateBoard(Board [[Red, Black, Black, Red, Black, Red, Black], [Black, Red, Black, Black, Black, Red, Black], [Red, Black, Red, Red, Red], [Black,Red, Red, Red, Black, Black, Red], [Black],[Red, Black],[Black]] Black) 6 ))
b4u3 = putStrLn (showBoard (updateBoard (Board [[Red, Black, Black, Red, Black, Red, Black], [Black, Red, Black, Black, Black, Red, Black], [Red, Black, Red, Red, Red], [Black,Red, Red, Red, Black, Black, Red], [Black],[Red, Black],[Black]] Red) 7))



