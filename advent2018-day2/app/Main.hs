module Main where

import Lib

main :: IO ()
main = do
    boxList <- readFile "input.txt"
    print . computeChecksum . words $ boxList
    print .findOneLetterDifferences . words $ boxList