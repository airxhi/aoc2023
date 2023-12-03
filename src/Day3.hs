module Day3 (day3) where

import Data.List.Split ( splitOn )
import Data.Char (isDigit)

day3 :: String -> IO ()
day3 inp = do
    let data2 = parseInput inp
    print $ part1 data2
    print $ part2 data2

part1 :: [String] -> Int
part1 = sum . map fst . findParts isAnySymbol

part2 :: [String] -> Int
part2 s =
    let
        gears = findGears s
        parts = findParts isGear s
        adjacentParts = getAdjacentParts parts gears
    in sum . map product . filter (\x -> length x == 2) $ adjacentParts

findParts :: (Char -> Bool) -> [String] -> [(Int, Coords)]
findParts f s = filterAdjacentSymbol f s . sanitizeCoords . map extractNumbers $ s

getAdjacentParts :: [(Int, Coords)] -> [(Int, Int)] -> [[Int]]
getAdjacentParts parts = map (map fst . (\(x,y) -> filter (\(v, (x1',x2',y')) -> isAdjacentSimple x y x1' x2' y') parts))

isAdjacentSimple :: (Eq a1, Eq a2, Num a1, Num a2, Enum a1, Enum a2) => a1 -> a2 -> a1 -> a1 -> a2 -> Bool
isAdjacentSimple x1 y x1' x2' y' = x1 `elem` [x1'-1..x2'+1] && y `elem` [y'-1..y'+1]

type Coords = (Int, Int, Int)

isAnySymbol :: Char -> Bool
isAnySymbol x = not (isPeriod x || isDigit x)

isGear :: Char -> Bool
isGear = (=='*')

isPeriod :: Char -> Bool
isPeriod = (=='.')

filterAdjacentSymbol :: (Char -> Bool) -> [String] -> [(a, Coords)] -> [(a, Coords)]
filterAdjacentSymbol f s = filter (\x -> hasAdjacent f (snd x) s)

hasAdjacent :: (Char -> Bool) -> Coords -> [String] -> Bool
hasAdjacent f (x1, x2, y) =
    let x1' = x1-1
        x2' = x2+1
    in any (any f . getAdjacentColumns x1' x2') . getAdjacentRows y

getAdjacentColumns :: Int -> Int -> [a] -> [a]
getAdjacentColumns x1 x2 = take (x2-x1+1) . drop x1

getAdjacentRows :: Int -> [String] -> [String]
getAdjacentRows y = take (if y == 0 then 2 else 3) . drop (y-1)

sanitizeCoords :: [[(Int, Int, Int)]] -> [(Int, Coords)]
sanitizeCoords = concatMap (\(y, xs) -> map (\(value, x1, x2) -> (value, (x1, x2, y))) xs) . snd . foldl (\(i, xs) x -> (i+1, xs++[(i,x)])) (0, [])

extractNumbers :: String -> [(Int, Int, Int)]
extractNumbers = map (\(a,b) -> (read a, b - length a, b - 1)) . findNumbers

findNumbers :: String -> [(String, Int)]
findNumbers = (\(i, l, xs) -> if l /= "" then xs ++ [(l,i)] else xs) . foldl (\(i, s, xs) c -> (i+1, if isDigit c then s++[c] else "", if not (isDigit c) && (s /= "") then xs++[(s, i)] else xs)) (0, "", [])

findGears :: [String] -> [(Int, Int)]
findGears = snd . foldl (\(i, acc) x -> (i+1, acc ++ findGearsLine i x)) (0, [])

findGearsLine :: Int -> String -> [(Int, Int)]
findGearsLine j = snd . foldl (\(i, xs) c -> (i+1, if isGear c then xs++[(i, j)] else xs)) (0, [])

parseInput :: String -> [String]
parseInput = filter (/= "") . splitOn "\n"

