module Lib
    (
        sumOfInputFile,
        firstDuplicateSum,
        findFirstDuplicates
    ) where

import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace

sumOfInputFile :: IO ()
sumOfInputFile = do
    integerList <- fileToIntegerList "input.txt"
    print . sum $ integerList

firstDuplicateSum :: IO ()
firstDuplicateSum = do
    fileContents <- fileToIntegerList "input.txt"
    let listOfSums = map (sumOfListUpTo fileContents) [1..]
    print . findFirstDuplicates $ listOfSums

findFirstDuplicates :: (Eq a, Show a) => [a] -> Maybe a
findFirstDuplicates xs = findDuplicatesHelper xs Nothing

findDuplicatesHelper :: (Eq a, Show a) => [a] -> Maybe a -> Maybe a
findDuplicatesHelper [] previous = previous
findDuplicatesHelper list previous =
    if elem (head list) (tail list) == False
        then findDuplicatesHelper (tail list) previous
        else
            let amountToTake = fromMaybe (length (tail list)) (elemIndex (head list) (tail list)) in
            findDuplicatesHelper (take amountToTake (tail list)) (Just (head list))

sumOfListUpTo :: [Int] -> Int -> Int
sumOfListUpTo list upTo = sum . take upTo . cycle $ list

fileToIntegerList :: FilePath -> IO [Int]
fileToIntegerList filePath = do
    contents <- readFile filePath
    return (map readInt . map stripLeadingPlus . words $ contents)

readInt :: String -> Int
readInt = read

stripLeadingPlus :: String -> String
stripLeadingPlus ('+':xs) = xs
stripLeadingPlus x = x

traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x