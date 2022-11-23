module Solver where
import ConnectFour
import Data.List
import Data.List.Split


------------------------------------------MILESTONE TWO-----------------------------------------------
{-For this milestone, you will need to be able to represent the board game in Haskell, make moves on 
the board game, and tell if a player has won the board game. -}

--The milestone description says it should return a Winner, not a Maybe Winner, but
--what if the player whose turn it is can't win or tie, it can only lose?
--Ask him if this is doing what it's supposed to be doing :(

swapColor :: Color -> Color
swapColor Red = Black
swapColor Black = Red

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