module Main (main) where

import Day1
import Day2
import Day3
import Day4

run :: (String -> IO()) -> String -> IO ()
run f fileName =
    do
        contents <- readFile fileName
        f contents

main :: IO ()
main = 
    do
        -- run day1 "inputs/1.txt"
        -- run day2 "inputs/2.txt"
        -- run day3 "inputs/3.txt"
        run day4 "inputs/4.txt"