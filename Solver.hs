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
whoWillWin (Board cols clr) =
    let movesLeft = availableMoves (Board cols clr)
        isWin = justToWinner (newCheckWinner (Board cols clr))
    in if isWin == YesWinner clr
       then YesWinner clr
       else let nextMvsWinLst = [whoWillWin (updateBoard (Board cols clr) x) |x <- movesLeft]
            in if YesWinner clr `elem` nextMvsWinLst
               then YesWinner clr
               else if Tie `elem` nextMvsWinLst
                    then Tie
                    else YesWinner (swapColor clr) --this is where the case for a loss should be
    where  
        justToWinner :: Maybe Winner -> Winner
        justToWinner (Just a) = a

bestMove :: Board -> Maybe Move
bestMove (Board cols clr) = 
    let possibleMvs = availableMoves (Board cols clr)
        possibleOutcomes = [(whoWillWin (updateBoard (Board cols clr) x), x) |x <- possibleMvs]
        winMatchesLst = [x |x <- possibleOutcomes, matchesWin x clr]
        tieMatchesLst = if (null winMatchesLst) then [ x| x <- possibleOutcomes, matchesTie x] else []
        --isTrue = foldr (\x y -> if(fst x == YesWinner clr) then True else y) False possibleOutcomes
    in if winMatchesLst /= []
       then Just (snd (head winMatchesLst))
       else if tieMatchesLst /= []
            then Just (snd (head tieMatchesLst))
            else if possibleOutcomes /= []
                 then Just (snd (head possibleOutcomes))
                 else Nothing 
    where 
        matchesWin :: (Winner, Move) -> Color -> Bool
        matchesWin (win, mv) clr = if win == (YesWinner clr) then True else False
        matchesTie :: (Winner, Move) -> Bool
        matchesTie (win, mv) = if win == Tie then True else False


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
        




