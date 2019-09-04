module Lib
    ( parseReport
    , parseMaybe
    , fillGuardNumbers
    , getAllGuardsNumbers
    , getReportsForGuard
    , computeTimeAsleep
    , computeMinuteArrays
    , findLargestMinuteArray
    , Report(Report)
    , ReportType(Begin, FallsAsleep, WakesUp)
    )
where

import           Text.ParserCombinators.ReadP
import           Data.Maybe
import           Data.List
import           Data.Time.Format
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Control.Applicative
import           Data.Ord
import qualified Data.Set                      as Set

anyChar :: ReadP Char
anyChar = satisfy (const True)

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input = case readP_to_S parser input of
    []                -> Nothing
    ((result, _) : _) -> Just result

reportTypeParser :: ReadP ReportType
reportTypeParser = do
    reportString <- manyTill anyChar (char ' ')
    if reportString == "Guard"
        then return Begin
        else if reportString == "falls"
            then return FallsAsleep
            else return WakesUp

guardNumberParser :: ReportType -> ReadP (Maybe String)
guardNumberParser Begin = do
    string "#"
    Just <$> manyTill anyChar (char ' ')
guardNumberParser _ = return Nothing

parseReport :: ReadP Report
parseReport = do
    dateTime <- between (char '[')
                        (char ']')
                        (readPTime False defaultTimeLocale "%Y-%m-%d %H:%M")
    string " "
    reportType  <- reportTypeParser
    guardNumber <- guardNumberParser reportType
    return (Report dateTime reportType guardNumber)

fillGuardNumbers :: [Report] -> [Report]
fillGuardNumbers = fillGuardNumbers' [] (Just "")
  where
    fillGuardNumbers' :: [Report] -> Maybe String -> [Report] -> [Report]
    fillGuardNumbers' result _ [] = result
    fillGuardNumbers' result guardNum (x : xs) =
        let repType = reportType x
            newGuardNumber =
                if repType == Begin then guardNumber x else guardNum
            newReport = Report (time x) (reportType x) newGuardNumber
        in  if isJust (guardNumber x)
                then fillGuardNumbers' (result ++ [x]) newGuardNumber xs
                else fillGuardNumbers' (result ++ [newReport]) newGuardNumber xs

getAllGuardsNumbers :: [Report] -> [Maybe String]
getAllGuardsNumbers = Set.toList . Set.fromList . map guardNumber

getReportsForGuard :: Maybe String -> [Report] -> [Report]
getReportsForGuard guardNum = filter (\x -> guardNumber x == guardNum)

computeTimeAsleep :: [Report] -> (Maybe String, Int)
computeTimeAsleep reports =
    ( guardNumber (head reports)
    , computeTimeAsleep' 0 (secondsToDiffTime 0) reports
    )
  where
    computeTimeAsleep' :: Int -> DiffTime -> [Report] -> Int
    computeTimeAsleep' totalTime _ [] = totalTime
    computeTimeAsleep' totalTime lastSleep (x : xs)
        | reportType x == Begin = computeTimeAsleep' totalTime lastSleep xs
        | reportType x == FallsAsleep = computeTimeAsleep'
            totalTime
            (utctDayTime . time $ x)
            xs
        | otherwise = computeTimeAsleep'
            (totalTime + round (((utctDayTime . time $ x) - lastSleep) / 60))
            lastSleep
            xs

computeMinuteArrays :: [Report] -> [(Maybe String, [Int])]
computeMinuteArrays = computeMinuteArrays' [] (secondsToDiffTime 0)
  where
    computeMinuteArrays'
        :: [(Maybe String, [Int])]
        -> DiffTime
        -> [Report]
        -> [(Maybe String, [Int])]
    computeMinuteArrays' arrays _ [] = arrays
    computeMinuteArrays' arrays lastAsleep (x : xs)
        | reportType x == Begin = computeMinuteArrays' arrays lastAsleep xs
        | reportType x == FallsAsleep = computeMinuteArrays'
            arrays
            (utctDayTime . time $ x)
            xs
        | otherwise = computeMinuteArrays'
            ( ( guardNumber x
              , [(floor lastAsleep `div` 60) .. (floor (utctDayTime . time $ x)
                                                `div` 60
                                                )
                  - 1]
              )
            : arrays
            )
            lastAsleep
            xs

findLargestMinuteArray :: [(Maybe String, [Int])] -> (Maybe String, (Int, Int))
findLargestMinuteArray [] = (Just "NULL", (0, 0))
findLargestMinuteArray arrays =
    (fst (head arrays), findLargestMinuteArray' arrays)
  where
    findLargestMinuteArray' arrs =
        let largestArray =
                maximumBy (\a b -> compare (length a) (length b)) (group (sort (concatMap snd arrs)))
        in (length largestArray, head largestArray)

data ReportType = Begin | FallsAsleep | WakesUp deriving (Show, Eq)

data Report = Report { time :: UTCTime
                     , reportType :: ReportType
                     , guardNumber :: Maybe String
                     } deriving (Show, Eq)

instance Ord Report where
    compare r1 r2 = compare (time r1) (time r2)
