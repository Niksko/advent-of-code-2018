module Lib
    ( 
        parseClaim,
        parseMaybe,
        Claim (Claim),
        overlaps,
        cartesianProduct
    ) where

import Text.ParserCombinators.ReadP
import Data.Maybe
import Data.List
import Control.Applicative

digit :: ReadP Char
digit = satisfy (\char -> char >= '0' && char <= '9')

numbersUntil :: ReadP a -> ReadP Int
numbersUntil until = fmap read (manyTill digit until)

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

parseClaim :: ReadP Claim
parseClaim = do
    string "#"
    claimId <- manyTill digit (string " ")
    string "@ "
    topLeftX <- numbersUntil (string ",")
    topLeftY <- numbersUntil (string ":")
    string " "
    width <- numbersUntil (string "x")
    height <- numbersUntil eof
    return (Claim
        claimId
        topLeftX
        topLeftY
        width
        height)

overlaps :: Claim -> Claim -> Bool
overlaps c1 c2 = not
    ((left c2 > right c1) ||
    (right c2 < left c1) ||
    (top c2 > bottom c1) ||
    (bottom c2 < top c1))

left :: Claim -> Int
left = topLeftX

right :: Claim -> Int
right c = left c + width c

top :: Claim -> Int
top = topLeftY

bottom :: Claim -> Int
bottom c = top c + height c

cartesianProduct :: Eq a => [a] -> [a] -> [(a,a)]
cartesianProduct as bs = [(a, b) | a <- as, b <- bs, a /= b]

data Claim = Claim
        { claimId :: String
        , topLeftX :: Int
        , topLeftY :: Int
        , width :: Int
        , height :: Int
        } deriving (Show, Eq)

instance Ord Claim where
    compare a b = compare (claimId a) (claimId b)
