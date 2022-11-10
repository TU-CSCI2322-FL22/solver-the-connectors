module Solver where
import ConnectFour
import Data.List


------------------------------------------MILESTONE TWO-----------------------------------------------
{-For this milestone, you will need to be able to represent the board game in Haskell, make moves on 
the board game, and tell if a player has won the board game. -}

data Outcome = Win | Tie deriving (Eq, Show)

