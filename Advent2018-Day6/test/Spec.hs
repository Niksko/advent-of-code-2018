import Test.Hspec
import Test.QuickCheck
import Lib

main :: IO ()
main = hspec $ do
    describe "someFunc" $ do
      it "Should do something" $ do
        someFunc "input" `shouldBe` "output"
