module Main where

import Lib
import Data.Maybe
import Data.List
import qualified Data.Set as Set

main :: IO ()
main = do
    reportListRaw <- readFile "input.txt"
    let reportList = mapMaybe (parseMaybe parseReport) . lines $ reportListRaw
    let sortedReportList = sort reportList
    let fullReportList = fillGuardNumbers sortedReportList
    let allGuardNumbers = getAllGuardsNumbers fullReportList
    let reportFunctions = map getReportsForGuard allGuardNumbers
    let guardReportLists = (map ($ fullReportList) reportFunctions)
    print . map computeTimeAsleep $ guardReportLists
    let allMinuteArrays = map computeMinuteArrays guardReportLists
    let largestArrays = map findLargestMinuteArray allMinuteArrays
    print largestArrays