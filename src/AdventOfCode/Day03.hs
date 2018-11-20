module AdventOfCode.Day03
  ( day03
  , Direction(..)
  ) where 

import Data.Functor (($>))
import Data.List
import System.IO
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec

data Direction
  = North
  | East
  | South
  | West
  deriving Show

parser :: Parser [Direction]
parser = many1 $ direction
  where
    direction :: Parser Direction
    direction =
      try (string "^" $> North) <|>
      (string ">" $> East) <|>
      (string "v" $> South) <|>
      (string "<" $> West)

    number :: Parser Int
    number = read <$> many1 digit

parseCSV :: String -> Either ParseError [Direction]
parseCSV input = parse parser "(unknown)" input

day03 :: IO ()
day03 = do  
    handle <- openFile "input/day-03.txt" ReadMode  
    contents <- hGetContents handle
    putStrLn "Part one"
    print $ (parseCSV contents)
    hClose handle  
