import Test.Hspec
import Test.QuickCheck
import Lib
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "parseClaim" $ do
    it "should correctly parse a valid claim" $ do
        parseMaybe parseClaim "#1 @ 1,3: 4x4" `shouldBe` Just (Claim "1" 1 3 4 4)

    it "should return Nothing for an invalid claim" $ do
        parseMaybe parseClaim "#1 a afjkll" `shouldBe` Nothing

    it "should parse complex claims" $ do
        parseMaybe parseClaim "#042421 @ 123,456: 111x999102" `shouldBe` Just (Claim "042421" 123 456 111 999102)