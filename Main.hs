module Main where
import ConnectFour
import Data.List
import Data.List.Split
import System.Console.GetOpt
import Data.Maybe
import Text.Read (readMaybe)
import System.IO
import System.Environment
import Data.Char

import Solver

main :: IO ()
main = 
  do args <- getArgs
     let (flags, inputs, error) = getOpt Permute options args
     let fname = if null inputs then "TestEvenMatch.hs" else head inputs
     contents <- readFile fname
     let bd = readGame contents
     if Help `elem` flags || (not $ null error)
     then putStrLn $ unlines error ++ usageInfo "Usage: ConnectFour [options] [file]" options
     else do
        (chooseAction flags bd)