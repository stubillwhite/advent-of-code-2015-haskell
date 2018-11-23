module AdventOfCode.Day01
  ( day01, finalFloor, stepsToBasement
  ) where 

import System.IO
import Data.List

currentFloor :: String -> [Int]
currentFloor s =
  scanl (\acc x -> acc + depthOffset x) 0 s
  where
    depthOffset '(' = 1
    depthOffset ')' = -1

finalFloor :: String -> Int
finalFloor = last . currentFloor

stepsToBasement :: String -> Maybe Int
stepsToBasement s = elemIndex (-1) (currentFloor s)
               
day01 :: IO ()
day01 = do  
    handle <- openFile "input/day-01.txt" ReadMode  
    contents <- hGetContents handle
    putStrLn "Part one"
    print $ finalFloor contents
    putStrLn "Part two"
    print $ stepsToBasement contents
    hClose handle  

