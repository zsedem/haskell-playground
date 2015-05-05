module Main where


import Test.Hspec(hspec)
import JSONSpec(spec)

main :: IO ()
main = hspec spec


