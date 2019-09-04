module Lib
    (
        computeChecksum,
        appearsNTimes,
        differsByOneLetter,
        commonLetters,
        findOneLetterDifferences,
        hasLettersThatAppearTwice,
        hasLettersThatAppearThrice
    ) where

import Data.List
import Data.Maybe

findOneLetterDifferences :: [String] -> (String, String)
findOneLetterDifferences list =
    let product = [(x, y) | x <- list, y <- list]
    in head (filter differsByOneLetter product)

differsByOneLetter :: (String, String) -> Bool
differsByOneLetter (str1, str2) =
    let zipped = zip str1 str2
    in (length . (mapMaybe tupleElemsAreNotEqual) $ zipped) == 1

tupleElemsAreNotEqual :: Eq a => (a, a) -> Maybe Bool
tupleElemsAreNotEqual (x, y) =
    if x /= y
        then Just True
        else Nothing

commonLetters :: String -> String -> String
commonLetters first second = intersect first second

computeChecksum :: [[Char]] -> Int
computeChecksum boxList =
    (countLettersAppearingTwice boxList) * (countLettersAppearingThrice boxList)

boolToCount :: Bool -> Int
boolToCount x = if x then 1 else 0

countLettersAppearingTwice :: [[Char]] -> Int
countLettersAppearingTwice boxList =
    sum . map (boolToCount . hasLettersThatAppearTwice) $ boxList

countLettersAppearingThrice :: [[Char]] -> Int
countLettersAppearingThrice boxList =
    sum . map (boolToCount . hasLettersThatAppearThrice) $ boxList

hasLettersThatAppearTwice :: [Char] -> Bool
hasLettersThatAppearTwice list =
    let boolList = map (appearsNTimes list 2) list
    in or boolList

hasLettersThatAppearThrice :: [Char] -> Bool
hasLettersThatAppearThrice list =
    let boolList = map (appearsNTimes list 3) list
    in or boolList

appearsNTimes :: [Char] -> Int -> Char -> Bool
appearsNTimes list n char = length (filter (\x -> x == char) list) == n