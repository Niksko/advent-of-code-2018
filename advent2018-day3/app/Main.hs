module Main where

import Lib
import Data.Maybe
import Data.List
import qualified Data.Set as S

main :: IO ()
main = do
    claimListRaw <- readFile "input.txt"
    let claimList = catMaybes . map (parseMaybe parseClaim) . lines $ claimListRaw
    let overlappingPairs = filter (\x -> overlaps (fst x) (snd x)) $ cartesianProduct claimList claimList
    let flatOverlappingPairs = S.fromList (concatMap (\x -> [fst x, snd x]) overlappingPairs)
    print . filter (\x -> S.notMember x flatOverlappingPairs) $ claimList
