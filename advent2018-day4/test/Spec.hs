import Test.Hspec
import Test.QuickCheck
import Data.Time.Format
import Lib
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "parseReport" $ do
    it "should correctly parse a valid report" $ do
        parseMaybe parseReport "[1518-11-01 00:00] Guard #10 begins shift" `shouldBe` Just (Report (parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M" "1518-11-01 00:00") Begin (Just "10"))
        parseMaybe parseReport "[1518-11-01 00:51] falls asleep" `shouldBe` Just (Report (parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M" "1518-11-01 00:51") FallsAsleep Nothing)
        parseMaybe parseReport "[1518-11-01 00:51] wakes up" `shouldBe` Just (Report (parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M" "1518-11-01 00:51") WakesUp Nothing)