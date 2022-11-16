module ConnectFour where
import Data.List
import Data.Aeson (Value(Bool))

------------------------------------------MILESTONE ONE-----------------------------------------------
{-For this milestone, you will need to be able to represent the maybe game in Haskell, make moves on 
the board game, and tell if a player has won the board game. -}

--Written by MTP below

data Board = Board [Column] Color deriving (Eq, Show) 
--Each Board data type has a list of columns and a color, the color indicates whose
--turn is next
--[Columns] and Color of player currently making move
--create an instance of Show for Board to show the current game state
data Color = Red | Black deriving (Eq, Show)
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
data Winner = YesWinner Color | Tie deriving (Eq, Show)
--noWinner == Nothing
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
                mkCol = foldr (\x y -> if findColor brd (cnt, x) == Just Red
                                    then '0' :y 
                                    else if findColor brd (cnt, x) == Just Black 
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
makeMove :: Board -> Move -> (Board, Maybe Winner)
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
--takes in a column and an int, and returns the color at that indexs (row)
getColorAtRow :: Column -> Int -> Maybe Color
getColorAtRow cl r
    |drop (r-1) cl == [] = Nothing
    |otherwise = Just (head (drop (r-1) cl))
--findColor takes a Board and a Coordinate and finds the Color at that coordinate
--If there is no color in the coordinate, or if that position is out of bounds, it will return neither
--NOTE: Our makeMove function must prevent out of bound moves
--(This doesn't check if it's out of bounds, it's just saying that if their is not a color in that
--position it must be either empty or out of bounds)
findColor :: Board -> Coordinate -> Maybe Color
findColor (Board cols clr) (x,y) = 
    let
        col = getColumn (Board cols clr) y
    in
        if null col || (x < 1 || y < 1)
        then Nothing
        else case (getColorAtRow col x) of Just Red -> (getColorAtRow col x)
                                           Just Black -> (getColorAtRow col x)
                                           Nothing -> Nothing


    
--takes board, Color we're checking for, coord of piece we're "at", direction we're going in, and a count
--returns a count of how many in a row of the color were checking there is in that Direction
countDir :: Board -> Color -> Coordinate -> Direction -> Int -> Int
countDir (Board cols cl) cChecking (row, col) (mvR, mvC) 4 = 4 
countDir (Board cols cl) cChecking (row, col) (mvR, mvC) cnt =
    let
        nextPos = (row + mvR, col + mvC)
        nextPosCol = findColor (Board cols cl) nextPos
    in
        if nextPosCol == Just cChecking
        then countDir (Board cols cl) cChecking nextPos (mvR, mvC) (cnt + 1)
        else cnt

--cD = (Board [[Red, Black, Red, Black, Red, Red], [Red, Black, Black, Red, Black, Red], [Black, Red, Black, Red, Black, Red], [],[],[],[]] Black) Black ()
fC = countDir (Board [[Red, Red, Red, Black, Red, Red], [Red, Black, Black, Red, Black, Red], [Black, Red, Black, Red, Black, Red], [],[],[],[]] Black) Red (2,1) (-1,0) 0
findC = findColor (Board [[Red, Red, Red, Black, Red, Red], [Red, Black, Black, Red, Black, Red], [Black, Red, Black, Red, Black, Red], [],[],[],[]] Black) (-1,1)

checkWinner :: Board -> Color -> Coordinate -> Maybe Winner
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
        then Just (YesWinner cChecking)
        else 
            if null mvsAvl
            then Just Tie
            else Nothing

--Going to assume that the color we pass in the Board tuple is the Color we're checking a winner for
--Otherwise we just change it
newCheckWinner :: Board -> Maybe Winner
newCheckWinner (Board [] color) = Nothing
newCheckWinner (Board cols color) = 
    let colWinLs = [checkOneCol x color |x <- cols, length x > 4]
        rowWinLs = [checkOneRow (Board cols color) x | x <- [1..6]]
    in  if True `elem` colWinLs || True `elem` rowWinLs
        then Just (YesWinner color)
        else Nothing
    where
        --use Int as a count in the recursion
        checkOneCol :: Column -> Color -> Bool --make it into aux to remove extra int
        checkOneCol col clr = aux col 0
            where
                aux :: Column -> Int -> Bool
                aux [] ct = ct >= 4
                aux [x] ct = x == clr && ct >= 3
                aux (x:xs) ct = if x == clr then aux xs (ct+1) else aux xs 0
        --Int in this situation is the row we're trying to find
        checkOneRow :: Board -> Int -> Bool
        checkOneRow (Board cols clr) r =
            let row = [getColorAtRow x r | x <- cols] --List of maybes
            in aux row 0
            where 
                aux :: [Maybe Color] -> Int -> Bool
                aux [] ct = ct >= 4
                aux [x] ct = x == Just clr && ct >= 3
                aux (x:xs) ct = if x == Just clr then aux xs (ct+1) else aux xs 0
        --Filter
        checkDiag :: Board -> Bool
        checkDiag (Board cols clr) = 
            let colsRowTups = [zip [1..] x |x <- cols]
            in findDiag colsRowTups clr 1
            where
                --Int is the column number :) Like always
                findDiag :: [[(Int, Color)]] -> Color -> Int -> Bool --make recursive so it ends when we find one diag
                findDiag cols clr colNum = -- if we get through the first four columns without finding a diag we won't find one in the last three
                    let col = head(drop (colNum - 1) cols)
                    in  if null col
                        then findDiag cols clr (colNum+1)
                        else True `elem`[spotDiagDsc x cols clr colNum |x <- col]
                        --ONLY CHECKING DSC RIGHT NOW
                spotDiagDsc :: (Int,Color) -> [[(Int, Color)]] -> Color -> Int -> Bool--Int being the col number
                spotDiagDsc (row, color) (c:cs) clr colNum = 
                    if color /= clr || row < 4 || colNum > 4
                    then False
                    else findNextSpotDsc (row, color) (c:cs) colNum 1
                --Takes a "spot", which is a row,color tuple, the Board represented as a row,color tuple,
                --an Int representing the column number of the spot, and an Int representing the count
                --of the same colors in this diag in a row
                --It then checks if the "descending" spot exists and if it's color matches the original spot
                --Returns true or false based on this, but should it return true or false based on cnt

                --we're gonna start with descending, so (-1,-1)
                --at count to findNextSpot to count if four in a row is met and make it recursive
                --If we made spotDiag recursive, it wouldn't check anything out side the possible 4 dsc diagonal area
                --do this for each spot in the area
                findNextSpotDsc :: (Int, Color) -> [[(Int, Color)]] -> Int -> Int ->  Bool
                findNextSpotDsc (row,clr) cols colNum count = 
                    let nextCols = drop colNum cols--what if not seven columns?
                        nextRow = row - 1
                        matchingSpots = [ x |x <- (head nextCols), (fst x) == nextRow]
                        noColsLeft = null nextCols
                        noMatchingSpots = null matchingSpots
                        colorsMatch = snd (head matchingSpots) /= clr
                    in if not ((noColsLeft || noMatchingSpots) || colorsMatch)
                       then if (count+1) >= 4 || findNextSpotDsc (nextRow, clr) cols (colNum+1) (count+1) 
                            then True 
                            else False
                       else False
                --I could have abstracted spot and findNext, but :( 
                --Could abstract later so that if Dir == (-1,-1) then check these parameters but I don't want to
                --Really can just use direction to abstract 
                spotDiagAsc :: (Int,Color) -> [[(Int, Color)]] -> Color -> Int -> Bool--Int being the col number
                spotDiagAsc (row, color) (c:cs) clr colNum = 
                    if color /= clr || row > 3 || colNum > 3
                    then False
                    else findNextSpotAsc (row, color) (c:cs) colNum 1
                findNextSpotAsc :: (Int, Color) -> [[(Int, Color)]] -> Int -> Int ->  Bool
                findNextSpotAsc (row,clr) cols colNum count = 
                    let nextCols = drop colNum cols--what if not seven columns?
                        nextRow = row + 1
                        matchingSpots = [ x |x <- (head nextCols), (fst x) == nextRow]
                        noColsLeft = null nextCols
                        noMatchingSpots = null matchingSpots
                        colorsMatch = snd (head matchingSpots) /= clr
                    in if not ((noColsLeft || noMatchingSpots) || colorsMatch)
                       then if (count+1) >= 4 || findNextSpotAsc (nextRow, clr) cols (colNum-1) (count+1) 
                            then True 
                            else False
                       else False 
        --asc = (1,-1)
        --dsc = (-1,-1)
        --assume head of head of colAssList is next to be checked
        --for each col
        --for each thing in that col
        --check for diag in either dir
        --zip with row position?
        



--Not sure if we need these:
showcolor (Red) = '0'
showColor (Black) = 'X'



--Tester Code--
--Test for row:

wDiagRedDsc = Board[[],[Black,Black,Red,Black,Red],[Black,Black,Red,Red],[Black,Black,Red],[Black,Red],[],[]] Red
wDiagBlackDsc = Board [[Red,Red,Red,Red,Red,Black],[Red,Red,Red,Red,Black],[Red,Red,Red,Black],[Red,Red,Black],[],[],[]] Black

wDiagFail = Board [[Red],[Red],[Red],[Red],[Red],[Red],[Red]] Red
wRR = Board [[Red,Red,Red],[Red,Black,Black],[Red,Black,Black],[Red,Black,Black],[Red,Black,Black]] Black
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
