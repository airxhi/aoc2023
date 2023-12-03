{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Day2 (day2) where

import Data.List.Split ( splitOn )
import Text.Parsec
    ( char, digit, spaces, string, many1, sepBy, (<|>), parse, try )
import Text.Parsec.String (Parser)
import qualified Data.Maybe
import Data.Maybe ( mapMaybe )

data Game = Game Int [Round]
  deriving Show

data Round = Round [(Int, Color)]
  deriving Show

data Color = Red | Green | Blue
    deriving (Show, Read)

day2 :: String -> IO ()
day2 inp = do
    let data2 = parseInput inp

    print $ part1 data2
    print $ part2 data2

part1 :: [Game] -> Int
part1 = sum . map (\(Game x _) -> x) . filter gameValid

gameValid :: Game -> Bool
gameValid (Game _ rounds) = all roundValid rounds

roundValid :: Round -> Bool
roundValid (Round xs) = all drawValid xs

drawValid :: (Int, Color) -> Bool
drawValid (x, Red) = x <= 12
drawValid (x, Green) = x <= 13
drawValid (x, Blue) = x <= 14

part2 :: [Game] -> Int
part2 = sum . map gamePower

gamePower :: Game -> Int
gamePower (Game _ rounds) = product . thrupleToList . getMaxColors . map toRgb $ rounds

thrupleToList :: (a, a, a) -> [a]
thrupleToList (a,b,c) = [a,b,c]

getMaxColors :: [(Int, Int, Int)] -> (Int, Int, Int)
getMaxColors = foldl (\(a', b', c') (a,b,c) -> (max a a', max b b', max c c')) (0,0,0)

toRgb :: Round -> (Int, Int, Int)
toRgb (Round xs) = (getColor matchRed xs, getColor matchGreen xs, getColor matchBlue xs)

getColor :: (Color -> Bool) -> [(Int, Color)] -> Int
getColor f = head . (++[0]) . map fst . filter (\(_, c) -> f c)

matchRed :: Color -> Bool
matchRed c = case c of 
    Red -> True
    _ -> False

matchGreen :: Color -> Bool
matchGreen c = case c of 
    Green -> True
    _ -> False

matchBlue :: Color -> Bool
matchBlue c = case c of 
    Blue -> True
    _ -> False

parseGame :: Parser Game
parseGame = do
  string "Game "
  gameId <- many1 digit
  char ':'
  spaces
  rounds <- parseRound `sepBy` char ';'
  return $ Game (read gameId) rounds

parseRound :: Parser Round
parseRound = do
  spaces
  items <- parseItem `sepBy` char ','
  return $ Round items

parseItem :: Parser (Int, Color)
parseItem = do
  spaces
  count <- many1 digit
  spaces
  color <- parseColor
  return (read count, color)

parseColor :: Parser Color
parseColor =
  try (string "green" >> return Green)
  <|> try (string "blue" >> return Blue)
  <|> try (string "red" >> return Red)

parseInput :: String -> [Game]
parseInput = mapMaybe runGameParser . splitLines

runGameParser :: String -> Maybe Game
runGameParser input = case parse parseGame "" input of
    Left err -> Nothing
    Right game -> Just game

splitLines :: String -> [String]
splitLines = filter (/= "") . splitOn "\n"
