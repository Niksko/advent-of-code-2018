module Main where

import Lib

main :: IO ()
main = do
    polymer <- readFile "input.txt"
    let completed = reactToCompletion polymer
    let charRemovers = map dropChars letterCombos
    let polymersWithCharsRemoved = map ($ polymer) charRemovers
    let removedLengths = map (length . reactToCompletion) polymersWithCharsRemoved
    print removedLengths