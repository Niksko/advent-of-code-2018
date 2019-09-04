import Test.Hspec
import Test.QuickCheck
import Lib
import qualified Data.Set as Set
import Control.Exception (evaluate)

prop_reactToCompletion_shouldPreserveParity :: String -> Bool
prop_reactToCompletion_shouldPreserveParity input
  | odd . length $ input = odd . length $ reactToCompletion input
  | otherwise = even . length $ reactToCompletion input

prop_reactOnce_shouldRemoveTwoOrZeroLetters :: String -> Bool
prop_reactOnce_shouldRemoveTwoOrZeroLetters input =
  let lengthDiff = length input - (length . reactOnce $ input)
  in lengthDiff == 2 || lengthDiff == 0

main :: IO ()
main = hspec $ do
    describe "reactOnce" $ do
      it "Should correctly apply the first reaction it finds" $ do
        reactOnce "aA" `shouldBe` ""
        reactOnce "aabAAB" `shouldBe` "aabAAB"
        reactOnce "abAB" `shouldBe` "abAB"
        reactOnce "dabAcCaCBAcCcaDA" `shouldBe` "dabAaCBAcCcaDA"
        reactOnce "dabAaCBAcCcaDA" `shouldBe` "dabCBAcCcaDA"
        reactOnce "dabCBAcCcaDA" `shouldBe` "dabCBAcaDA"
        reactOnce "dabCBAcaDA" `shouldBe` "dabCBAcaDA"

      it "Should only remove two letters at a time or no letters" $ property prop_reactOnce_shouldRemoveTwoOrZeroLetters

    describe "reactToCompletion" $ do
      it "Should correctly react to completion" $ do
        reactToCompletion "dabAcCaCBAcCcaDA" `shouldBe` "dabCBAcaDA"
        reactToCompletion "abBA" `shouldBe` ""

      it "Should preserve parity" $ property prop_reactToCompletion_shouldPreserveParity

    describe "dropChars" $
        it "Should remove the specified chars" $ do
          dropChars (Set.fromList "abc") "abcdefg" `shouldBe` "defg"
          dropChars (Set.fromList "abcdef") "abcdefg" `shouldBe` "g"
          dropChars (Set.fromList "abcdef") "abcdefgg" `shouldBe` "gg"
          dropChars (Set.fromList "abcdef") "abcdefabcdefgg" `shouldBe` "gg"