module Day1 (day1) where


import Data.List.Split ( splitOn )
import Data.Char (isAlpha, digitToInt, intToDigit)
import Data.List.Utils ( replace )

digitToWord :: Int -> String
digitToWord n =
    case n of
        1 -> "one"
        2 -> "two"
        3 -> "three"
        4 -> "four"
        5 -> "five"
        6 -> "six"
        7 -> "seven"
        8 -> "eight"
        9 -> "nine"

digitToPain :: (Eq a, Num a) => a -> String
digitToPain n =
    case n of
        1 -> "o1e"
        2 -> "t2o"
        3 -> "t3e"
        4 -> "4"
        5 -> "5e"
        6 -> "6"
        7 -> "7n"
        8 -> "e8t"
        9 -> "n9e"

numbers :: [(String, String)]
numbers = map (\x -> (digitToPain x, digitToWord x)) [1..9]


day1 :: String -> IO ()
day1 inp = do
    let data2 = parseInput inp
    print $ part1 data2
    print $ part2 data2

part1 :: [String] -> Int
part1 = sum . map (concatDigits . getFirstAndLast . getDigits)

part2 :: [String] -> Int
part2 = part1 . map replaceNumbers 

getFirstAndLast :: [Int] -> [Int]
getFirstAndLast xs = [head xs, last xs]

getDigits :: [Char] -> [Int]
getDigits = map digitToInt . filter (not . isAlpha)

concatDigits :: [Int] -> Int
concatDigits = read . map intToDigit

replaceNumbers :: String -> String
replaceNumbers xs = foldl (\acc (n, s) -> replace s n acc) xs numbers

parseInput :: String -> [String]
parseInput = filter (/= "") . splitOn "\n"