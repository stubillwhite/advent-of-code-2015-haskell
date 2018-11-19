module AdventOfCode.Day02
  ( day02
  ) where 

import System.IO
import Data.List
import Text.ParserCombinators.Parsec

data Parcel = Parcel
  { x :: Int
  , y :: Int
  , z :: Int
  } deriving (Show)


parser :: Parser [Parcel]
parser = many1 $ parcel <* newline
  where
    parcel :: Parser Parcel
    parcel = Parcel <$> (number <* char 'x') <*> (number <* char 'x') <*> (number)

    number :: Parser Int
    number = read <$> many1 digit

parseCSV :: String -> Either ParseError [Parcel]
parseCSV input = parse parser "(unknown)" input

day02 :: IO ()
day02 = do  
    handle <- openFile "input/day-02.txt" ReadMode  
    contents <- hGetContents handle
    putStrLn "Part one"
    print $ parseCSV contents 
    hClose handle  

