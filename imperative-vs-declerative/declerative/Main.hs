module Main where

main :: IO ()
main = print . sum . map readInt . lines =<< readFile "numbers.txt"

readInt :: String -> Int
readInt = read

