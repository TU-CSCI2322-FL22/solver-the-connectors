module Solver where
import ConnectFour
import Data.List
import Data.List.Split


------------------------------------------MILESTONE TWO-----------------------------------------------
{-For this milestone, you will need to be able to represent the board game in Haskell, make moves on 
the board game, and tell if a player has won the board game. -}


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

