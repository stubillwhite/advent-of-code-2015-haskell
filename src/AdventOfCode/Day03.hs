module AdventOfCode.Day03
  ( solutionPartOne
  , day03
  ) where 

import Data.Functor (($>))
import Data.List
import qualified Data.Map as Map (Map(..), findWithDefault, insertWith, empty, filter, keys)
import System.IO
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec

data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq)

data Location = Location
  { x :: Int
  , y :: Int
  }
  deriving (Show, Eq, Ord)

instance Semigroup Location where
   (<>) (Location x1 y1) (Location x2 y2) = Location (x1 + x2) (y1 + y2)

deliverTo :: Location -> Direction -> Location
deliverTo location direction = location <> (delta direction)

delta :: Direction -> Location
delta North = Location 0 (-1)
delta East = Location 1 0
delta South = Location 0 1
delta West = Location (-1) 0

countOccurrences :: Ord k => [k] -> Map.Map k Int
countOccurrences coll
 = foldl (\acc x -> incrementCount acc x) Map.empty coll
  where
    incrementCount acc x = Map.insertWith (+) x 1 acc

locationsVisited :: [Direction] -> [Location]
locationsVisited directions =
  scanl (\acc x -> acc <> (delta x)) (Location 0 0) directions

receiveAtLeastOnePresent :: [Location] -> [Location]
receiveAtLeastOnePresent locations =
  Map.keys $ Map.filter (\x -> x >= 1) $ countOccurrences locations 
  
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

solutionPartOne :: String -> Either ParseError Int
solutionPartOne directions = 
  (fmap (length . receiveAtLeastOnePresent . locationsVisited) (parseCSV directions))

day03 :: IO ()
day03 = do  
    handle <- openFile "input/day-03.txt" ReadMode
    contents <- hGetContents handle
    putStrLn "Part one"
    print $ solutionPartOne contents
    hClose handle  
