module AdventOfCode.Day02
  ( day02
  , Parcel(..)
  , paperRequiredToWrap
  , totalSquareFeetToWrap
  ) where 

import System.IO
import Data.List
import Text.ParserCombinators.Parsec

data Parcel = Parcel
  { l :: Int
  , w :: Int
  , h :: Int
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

paperRequiredToWrap :: Parcel -> Int
paperRequiredToWrap (Parcel l w h) =
  (2 * l * w) + (2 * w * h) + (2 * h * l) + slack
  where
    slack = minimum [l * w, l * h, w * h]

totalSquareFeetToWrap :: [Parcel] -> Int
totalSquareFeetToWrap parcels =
  foldl (+) 0 (map paperRequiredToWrap parcels)

day02 :: IO ()
day02 = do  
    handle <- openFile "input/day-02.txt" ReadMode  
    contents <- hGetContents handle
    putStrLn "Part one"
    print $ (fmap totalSquareFeetToWrap (parseCSV contents))
    hClose handle  

