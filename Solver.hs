module Solver where
import ConnectFour
import Data.List
import Data.List.Split
import System.Console.GetOpt
import Data.Maybe
import Text.Read (readMaybe)
import System.IO
import System.Environment
import Data.Char


------------------------------------------MILESTONE TWO-----------------------------------------------
{-For this milestone, you will need to be able to represent the board game in Haskell, make moves on 
the board game, and tell if a player has won the board game. -}

--The milestone description says it should return a Winner, not a Maybe Winner, but
--what if the player whose turn it is can't win or tie, it can only lose?
--Ask him if this is doing what it's supposed to be doing :(

swapColor :: Color -> Color
swapColor Red = Black
swapColor Black = Red

--The range of the score is 60 to -60, where 60 is a win for player one, -60 is a win for player two, and
--0 is a tie.
type Score = Int
--cutOffDepth returns Move
--DON't FORGET TO UPDATEBOARD
--should return Maybe Move? (or Move)
cutOffSearch :: Board -> Int -> Score --return highest score
cutOffSearch brd@(Board cols clr) cutDepth = 
    let movesLeft = availableMoves brd
    in case (newCheckWinner brd) of 
            Just outcome -> if outcome == YesWinner clr 
                            then 60 
                            else if outcome == Tie 
                                 then 0 
                                 else -60
            Nothing -> if cutDepth == 0 
                       then evaluate brd
                       else maximum [cutOffSearch (updateBoard brd x) (cutDepth - 1) |x <- movesLeft]

--search for a move that forces the game to the best board state within the cut-off depth
cutOffBestMove :: Board -> Int -> Maybe Move
cutOffBestMove brd@(Board cols clr) depth = 
    let possibleMvs = availableMoves (Board cols clr)
        possibleOutcomes = [(cutOffSearch (updateBoard (Board cols clr) x) depth, x) |x <- possibleMvs]
        --if there are no possible moves (could be a tie), return nothing 
        --isTrue = foldr (\x y -> if(fst x == YesWinner clr) then True else y) False possibleOutcomes
    in bestMoveFor possibleOutcomes --just fold to return the move with the highest score
    where bestMoveFor :: [(Score, Move)] -> Maybe Move
          bestMoveFor outs =
                case lookup 60 outs of 
                  Just move -> Just move
                  Nothing -> snd (foldl (\acc (x,y) -> if x > fst acc 
                                              then (x, Just y)
                                              else acc ) (-60, Nothing) outs)
                    

evaluate :: Board -> Int 
evaluate brd@(Board cols clr) =
    let isWonByOne = newCheckWinner brd
        oppClr = swapColor clr
    in if isWonByOne == Just (YesWinner clr) 
       then 60
       else if isWonByOne == Just Tie 
            then 0
            else if newCheckWinner (Board cols oppClr) == Just (YesWinner oppClr) 
                 then -60
                else findBoardScore brd
    where findBoardScore :: Board -> Int
          findBoardScore brd@(Board cols clr) = 
            let oppClr = swapColor clr
                oppBoard = Board cols oppClr
                scoreOne = sum [ countAllDirs brd (length (getColumn brd x), x)  | x <- [1..7]]
                scoreTwo = sum [ countAllDirs oppBoard (length (getColumn oppBoard x), x)  | x <- [1..7]]
            in if scoreOne > scoreTwo 
               then scoreOne
               else if scoreTwo > scoreOne 
                    then -scoreTwo
                    else 0
          countAllDirs :: Board -> Coordinate -> Int
          countAllDirs brd@(Board cols clr) coord =
            let dirs = [(-1,1),(1,1),(-1,0),(0,1)]--rtDsc, rtAsc, down, right
            in sum [dirCounter brd coord x | x <- dirs]
            
 --Evaluate will take a board. It will check if either color has one the game, then it
          --will only check for wins in valid directions starting from the top most piece in 
          --each column.
          --If four in a row are found for the color passed in with the board (player one), it will 
          --return 4 and stop checking. Else, it will find the highest number of consecutive pieces for Player
          --One and Player Two. It will return the number with the greatest value.
          --The return range is 4 to -4.
--Assumes we're checking for color cl
--ACTUALLY NO LIMITATIONS BECAUSE WE'RE NOT JUST CHECKING FOR FOUR IN A ROW BUT JUST A COUNT
dirCounter :: Board -> Coordinate -> Direction -> Int
dirCounter brd@(Board cols cl) (row, col) (mvR, mvC) =
    aux brd (row,col) (mvR, mvC) 0
    where
        aux :: Board -> Coordinate -> Direction -> Int -> Int
        aux (Board cols cl) (row, col) (mvR, mvC) 4 = 4 
        aux (Board cols cl) (row, col) (mvR, mvC) cnt =
            let
                nextPos = (row + mvR, col + mvC)
                nextPosCol = findColor (Board cols cl) nextPos
            in
                if nextPosCol == Just cl
                then countDir (Board cols cl) cl nextPos (mvR, mvC) (cnt + 1)
                else cnt


--Takes a Board, and if a win is possible it returns 'YesWinner Color', if a win isn't
--possible but a tie is, it returns 'Tie'
--Else, it returns that the opposite color will win
whoWillWin :: Board -> Winner
whoWillWin brd@(Board cols clr) =
    let movesLeft = availableMoves brd
        nextMvsWinLst = [whoWillWin (updateBoard brd x) |x <- movesLeft]
    in case (newCheckWinner brd) of 
            Just outcome -> outcome
            Nothing -> bestOutcomeFor nextMvsWinLst clr--if isWin then return that else handle else
    where bestOutcomeFor :: [Winner] -> Color -> Winner
          bestOutcomeFor outcomes colr  
               | YesWinner colr `elem` outcomes = YesWinner colr
               | Tie `elem` outcomes = Tie
               | otherwise = YesWinner (swapColor colr)


bestMove :: Board -> Maybe Move
bestMove (Board cols clr) = --make sure to check if it's already one, or possibleMvs is null
    --use built in function
    let possibleMvs = availableMoves (Board cols clr)
        possibleOutcomes = [(whoWillWin (updateBoard (Board cols clr) x), x) |x <- possibleMvs]
        --if there are no possible moves (could be a tie), return nothing 
        --isTrue = foldr (\x y -> if(fst x == YesWinner clr) then True else y) False possibleOutcomes
    in bestMoveFor possibleOutcomes clr
    where 
        bestMoveFor :: [(Winner, Move)] -> Color -> Maybe Move
        bestMoveFor outs clr =
            case lookup (YesWinner clr) outs of 
                Just move -> Just move
                Nothing -> case lookup (Tie) outs of
                               Just move -> Just move
                               Nothing -> case outs of
                                            [] -> Nothing
                                            _ -> Just (snd (head outs))
      

charToColor :: Char -> Color
charToColor 'X' = Black
charToColor '0' = Red

readGame :: String -> Board 
readGame str = 
    let clr = charToColor(head(str))
        clmString = tail(str)
        newlst = (splitOn "\n" clmString)
        clmslst = reverse(tail (reverse (foldr( \x y -> [charToColor g | g <- x]:y) [] newlst)))
    in Board clmslst clr

showGame :: Board -> String
showGame (Board clms clr) =
    showColor(clr):aux clms
    where 
        aux [] = []
        aux (x:xs) = [showColor(c) | c <- x]++ '\n': aux xs

writeGame :: Board -> FilePath -> IO ()
writeGame bd fp = 
    let str = showGame bd
    in writeFile fp str

loadGame :: FilePath -> IO Board 
loadGame fp = do
    contents <- readFile fp
    return (readGame contents)

putWinner :: Board -> IO ()
putWinner bd = 
    let winner = checkWinner bd Red (1,1)
    in putStrLn("The winner is " ++ aux(winner))
    where 
        aux :: Maybe Winner -> String
        aux x = case x of 
            Just value -> justval value
            Nothing -> "no one."
        justval :: Winner -> String
        justval (YesWinner x) = [showColor(x)]
        justval (Tie) = "no one"

data Flag = Help | Winner | Move String | Depth String | Verbose deriving (Eq, Show)

options :: [OptDescr Flag]
options = [Option ['h'] ["help"] (NoArg Help) "Print usage information and exit."
          , Option ['w'] ["winner"] (NoArg Winner) "Prints the best move using an exhaustive search (no cut off depth)."
          , Option ['m'] ["move"] (ReqArg Move "m") "Make m and print out the resulting board, in the input format, to stdout. The move should be 1-indexed. If a move requires multiple values, the move will be a tuple of numbers separated by a comma with no space."
          , Option ['d'] ["depth"] (ReqArg Depth "#") "Uses # as a cutoff depth, instead of the default."
          , Option ['v'] ["verbose"] (NoArg Verbose) "Output both the move and a description of how good it is: win, lose, tie, or a rating."
          ]

getMove :: [Flag] -> Maybe Move --modify for maybe
getMove [] = Nothing
getMove ((Move x):_) =
    case readMaybe x of 
        Nothing -> error "Invalid input to move flag"
        Just move -> Just move
getMove (_:flags) = getMove flags

getDepth :: [Flag] -> Int
getDepth [] = 3
getDepth ((Depth x):_) =
    case readMaybe x of 
        Nothing -> error "Invalid input to depth flag"
        Just depth -> depth
getDepth (_:flags) = getDepth flags

main :: IO ()
main = 
  do args <- getArgs
     let (flags, inputs, error) = getOpt Permute options args
     let fname = if null inputs then "default.txt" else head inputs
     contents <- readFile fname
     let bd = readGame contents
     --let bd = loadGame fname
     if Help `elem` flags || (not $ null error)
     then putStrLn $ usageInfo "Usage: ConnectFour [options] [file]" options
     else do
        (chooseAction flags bd)

chooseAction :: [Flag] -> Board -> IO ()
chooseAction flags bd
  | Winner `elem` flags = tellBestMove bd
  | checkForMoveInFlags flags = makeAndTellMove bd flags
  | Winner `notElem` flags = tellMoveWithCutOffDepth bd flags 
  | otherwise = tellVerbose bd -- if Verbose `elem` flags == true 

tellMoveWithCutOffDepth :: Board -> [Flag] -> IO ()
tellMoveWithCutOffDepth bd flags = do
    let gd = getDepth flags
    putStrLn ("hi") --("The best move is " ++ show(fromJust(cutOffSearch gd bd)) ++ ".")) --cutOffSearch
    
tellVerbose :: Board -> IO () 
tellVerbose bd = 
    let bm = bestMove bd
    in if bm == Nothing 
       then putStrLn("No best move.")
       else putStrLn("The best move is " )-- ++ show(fromJust(bm)) ++ ". The rating of the move is " ++ show(evaluateMove (fromJust bm)) ++ ".")

tellBestMove :: Board -> IO ()
tellBestMove bd = do
    let bm = bestMove bd
    if bm == Nothing 
    then putStrLn("No best move.")
    else putStrLn("The best move is " ++ show(fromJust(bm)) ++ ".")

makeAndTellMove :: Board -> [Flag] -> IO ()
makeAndTellMove bd flags = do
    let gm = getMove flags
    let newbd = updateBoard bd (fromJust (gm))
    putStrLn (showGame newbd)

checkForMoveInFlags :: [Flag] -> Bool
checkForMoveInFlags flags = 
    let gm = getMove flags
    in if gm == Nothing then False
       else True

--Tester Code Below--
rg = readGame "X000X00\n0XX0X0\nX0X0X0\n\n\n\n\n"
sg = showGame (Board [[Red, Red, Red, Black, Red, Red], [Red, Black, Black, Red, Black, Red], [Black, Red, Black, Red, Black, Red], [],[],[],[]] Black)
wg = writeGame (Board [[Red, Red, Red, Black, Red, Red], [Red, Black, Black, Red, Black, Red], [Black, Red, Black, Red, Black, Red], [],[],[],[]] Black) "newfile.hs"
lg = loadGame "newfile.hs"
pw = putWinner (Board [[Red, Red, Red, Black, Red, Red], [Red, Black, Black, Red, Black, Red], [Black, Red, Black, Red, Black, Red], [],[],[],[]] Black)

wWW1 = whoWillWin (Board [[Black, Black,Red,Black,Black],[Black, Red, Red, Black,Black, Red], [Black, Red,Black, Red], [Red, Red,Black, Red,Red,Black], [Black,Black,Red,Black, Red], [Black, Red, Black, Red,Black, Red],[Black,Red,Black,Black,Black]] Red) --should return Winner Red
wWW2 = whoWillWin (Board [[Red, Red, Red, Black,Black], [Black,Red,Black,Black,Red], [Red,Black,Red,Red,Red,Black],[Red,Black,Black, Black,Red], [],[Black,Black,Black, Red], [Red,Red, Red, Black]] Black) --Should return Winner Black. Black will win regardless of where Red makes its next move.
wWW3 = whoWillWin (Board [[Black, Black, Black, Red, Red], [Red, Black, Red, Red, Black], [Black, Red, Black, Black, Black, Red], [Black, Red, Red, Red, Black], [],[Red, Red, Red, Black], [Black,Black,Black,Red]] Red) --Should return Winner Red. Red will win regardless of where Black makes its next move. 
wWW4 = whoWillWin (Board [[Black, Black,Red,Black,Black,Red],[Black, Red, Red, Black,Black, Red], [Black, Red,Black, Red,Black,Black], [Red, Red,Black, Red,Red,Black], [Black,Black,Red,Black, Red], [Black, Red, Black, Red,Black, Red],[Black,Red,Black,Black,Black,Red]] Red) --should return Tie.
wWW5 = whoWillWin (Board [[Black, Black, Black, Red, Red,Black], [Red, Black, Red, Red, Black,Red], [Black, Red, Black, Black, Black, Red], [Black, Red, Red, Red, Black], [],[Red, Red, Red, Black], [Black,Black,Black,Red]] Red) --should be Red.
wWW6 = whoWillWin (Board [[Black, Red, Black, Red, Black, Red],[Black, Red, Black, Red, Black, Red],[Black, Red, Black, Red, Black, Red],[Red,Black,Red,Black,Red,Black], [Black, Red, Black, Red, Black, Red],[Black, Red, Black, Red, Black, Red],[Black, Red, Black, Red, Black,Red]] Red) --should be a Tie.
wWW7 = whoWillWin (Board [[Black, Red, Black, Red, Red,Red], [Red, Black, Red, Black,Black, Black], [Black,Red, Black, Red, Red, Red], [Black,Black,Black, Red, Black, Black], [Red,Red,Red,Black,Red,Black],[Black,Black,Red,Black,Red, Red],[Red,Red,Black,Red, Black,Black]] Red)-- should be a Tie.

bM1 = bestMove (Board [[Black, Black,Red,Black,Black],[Black, Red, Red, Black,Black, Red], [Black, Red,Black, Red], [Red, Red,Black, Red,Red,Black], [Black,Black,Red,Black, Red], [Black, Red, Black, Red,Black, Red],[Black,Red,Black,Black,Black]] Red)
bM2 = bestMove (Board [[Red, Red, Red, Black,Black], [Black,Red,Black,Black,Red], [Red,Black,Red,Red,Red,Black],[Red,Black,Black, Black,Red], [],[Black,Black,Black, Red], [Red,Red, Red, Black]] Black)
bM3 = bestMove (Board [[Black, Black, Black, Red, Red], [Red, Black, Red, Red, Black], [Black, Red, Black, Black, Black, Red], [Black, Red, Red, Red, Black], [],[Red, Red, Red, Black], [Black,Black,Black,Red]] Red)
bM4 = bestMove (Board [[Black, Black,Red,Black,Black,Red],[Black, Red, Red, Black,Black, Red], [Black, Red,Black, Red,Black,Black], [Red, Red,Black, Red,Red,Black], [Black,Black,Red,Black, Red], [Black, Red, Black, Red,Black, Red],[Black,Red,Black,Black,Black,Red]] Red)
bM5 = bestMove (Board [[Black, Black, Black, Red, Red,Black], [Red, Black, Red, Red, Black,Red], [Black, Red, Black, Black, Black, Red], [Black, Red, Red, Red, Black], [],[Red, Red, Red, Black], [Black,Black,Black,Red]] Red)
bM6 = bestMove (Board [[Black, Red, Black, Red, Black, Red],[Black, Red, Black, Red, Black, Red],[Black, Red, Black, Red, Black, Red],[Red,Black,Red,Black,Red,Black], [Black, Red, Black, Red, Black, Red],[Black, Red, Black, Red, Black, Red],[Black, Red, Black, Red, Black,Red]] Red)
bM7 = bestMove (Board [[Black, Red, Black, Red, Red,Red], [Red, Black, Red, Black,Black, Black], [Black,Red, Black, Red, Red, Red], [Black,Black,Black, Red, Black, Black], [Red,Red,Red,Black,Red,Black],[Black,Black,Red,Black,Red, Red],[Red,Red,Black,Red, Black,Black]] Red)


--Boards--
dominatedByRed = putStrLn (showBoard (Board [[Black, Black,Red,Black,Black],[Black, Red, Red, Black,Black, Red], [Black, Red,Black, Red], [Red, Red,Black, Red,Red,Black], [Black,Black,Red,Black, Red], [Black, Red, Black, Red,Black, Red],[Black,Red,Black,Black,Black]] Red))
dBR = writeGame (Board [[Black, Black,Red,Black,Black],[Black, Red, Red, Black,Black, Red], [Black, Red,Black, Red], [Red, Red,Black, Red,Red,Black], [Black,Black,Red,Black, Red], [Black, Red, Black, Red,Black, Red],[Black,Red,Black,Black,Black]] Red) "TestDominatedByRed.hs"


nearStart = putStrLn (showBoard(Board [[Black],[Red, Black], [Red, Black, Red], [Black, Red], [Red, Black], [Red, Red], [Black,Black]] Red))
nS = writeGame (Board [[Black],[Red, Black], [Red, Black, Red], [Black, Red], [Red, Black], [Red, Red], [Black,Black]] Red) "TestNearStart.hs"

evenMatch = putStrLn (showBoard (Board [[Red,Red,Black,Black], [Black, Black, Red, Red], [Red,Red,Black, Black], [Black, Black,Red,Red], [Red,Red,Black,Black], [Black, Black, Red, Red], [Red, Red, Black, Black]] Red))
eMa = writeGame (Board [[Red,Red,Black,Black], [Black, Black, Red, Red], [Red,Red,Black, Black], [Black, Black,Red,Red], [Red,Red,Black,Black], [Black, Black, Red, Red], [Red, Red, Black, Black]] Red) "TestEvenMatch.hs"

nearEnd = putStrLn (showBoard (Board [[Black,Black,Black, Red], [Red, Red, Red, Black, Black], [Red, Black,Black, Red], [Red, Red, Red,Black, Red], [Black,Black,Black,Red], [Black,Red,Red,Red, Black], []] Black))
nE = writeGame (Board [[Black,Black,Black, Red], [Red, Red, Red, Black, Black], [Red, Black,Black, Red], [Red, Red, Red,Black, Red], [Black,Black,Black,Red], [Black,Red,Red,Red, Black], []] Black) "TestNearEnd.hs"
