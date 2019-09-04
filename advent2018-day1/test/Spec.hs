import Test.Hspec
import Test.QuickCheck
import Lib
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "findDuplicates" $ do
    it "should return Nothing for a list with no duplicates" $ do
      findFirstDuplicates [1..10] `shouldBe` Nothing

    it "should find simple duplicates" $ do
      findFirstDuplicates [1, 2, 1] `shouldBe` Just 1

    it "should find a more complex duplicate" $ do
      findFirstDuplicates [1, 2, 3, 4, 5, -1, -2, 2] `shouldBe` Just 2

    it "should work on infinite lists" $ do
      findFirstDuplicates (cycle [1..5]) `shouldBe` Just 1

    it "should work" $ do
      findFirstDuplicates [0,3,6,10,8,4,7,10,14] `shouldBe` Just 10

    -- it "returns the first element of an *arbitrary* list" $
    --   property $ \x xs -> head (x:xs) == (x :: Int)

    -- it "throws an exception if used with an empty list" $ do
    --   evaluate (head []) `shouldThrow` anyException