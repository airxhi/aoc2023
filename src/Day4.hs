module Day4 (day4) where


import Data.List.Split ( splitOn )
import Data.Char (isAlpha, digitToInt, intToDigit)
import Data.List.Utils ( replace )
import Text.Parsec
    ( char, digit, spaces, string, count, many1, parse, ParseError )
import Text.Parsec.String (Parser)
import Data.Either (fromRight)
import Data.List (intersect)


day4 :: String -> IO ()
day4 inp = do
    let data2 = map parseCard (parseInput inp)
    print $ part1 data2
    print $ part2 data2

data Card = Card Int [Int] [Int]
  deriving Show

part1 :: [Card] -> Int
part1 = sum . map (\x -> (2^x) `div` 2) . getMatchingCards

part2 :: [Card] -> Int
part2 cards =
    let card_values = zip (map (\(Card x _ _) -> x) cards) (getMatchingCards cards)
    in sum $ chainCards card_values

chainCards :: (Foldable t, Num a) => t (Int, Int) -> [a]
chainCards xs = foldl (\acc (i, v) -> combine (+) acc (take (length xs) (newList i v (acc !! (i-1))))) (initializeOnes (length xs)) xs

combine :: (a -> a -> a) -> [a] -> [a] -> [a]
combine f xs ys =
    let 
        xLen = length xs
        yLen = length ys
    in 
        if xLen == yLen then zipWith f xs ys
        else if xLen > yLen then zipWith f xs ys ++ drop yLen xs
        else zipWith f xs ys ++ drop xLen ys

newList :: Num a => Int -> Int -> a -> [a]
newList offset num v = replicate offset 0 ++ replicate num v

initializeOnes :: Num a => Int -> [a]
initializeOnes n = replicate n 1

getMatchingCards :: [Card] -> [Int]
getMatchingCards = map (length . \(Card _ xs ys) -> xs `intersect` ys)

cardParser :: Parser Card
cardParser = do
  string "Card"
  spaces
  cardNumber <- read <$> many1 digit
  char ':'
  spaces
  values1 <- parseValues
  char '|'
  spaces
  Card cardNumber values1 <$> parseValues

parseValues :: Parser [Int]
parseValues = many1 (read <$> many1 digit <* spaces)

parseCard :: String -> Card
parseCard = fromRight (Card 0 [] []) . parse cardParser ""

parseInput :: String -> [String]
parseInput = filter (/= "") . splitOn "\n"