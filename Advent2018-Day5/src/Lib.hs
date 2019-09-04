module Lib
    (
        reactOnce,
        reactToCompletion,
        dropChars,
        letterCombos
    ) where

import Data.Char
import qualified Data.Set as Set

reactOnce :: String -> String
reactOnce (a:b:rest)
    | (isUpper a && isLower b && (a == toUpper b)) || (isLower a && isUpper b && (toUpper a == b)) = rest
    | otherwise = a:reactOnce (b:rest)
reactOnce [a] = [a]
reactOnce [] = []

reactToCompletion :: String -> String
reactToCompletion input =
    let new = reactOnce input
    in if new == input then input else reactToCompletion new

dropChars :: Set.Set Char -> String -> String
dropChars charsToDrop (x:xs) =
    if Set.member x charsToDrop
    then dropChars charsToDrop xs
    else x:dropChars charsToDrop xs
dropChars _ [] = []

allLetters = ['a'..'z']
letterCombos = map (\x -> Set.fromList [x, toUpper x]) allLetters