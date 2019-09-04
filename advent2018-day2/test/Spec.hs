import Test.Hspec
import Test.QuickCheck
import Lib
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "hasLettersThatAppearTwice" $ do
    it "should return the correct value as per the spec" $ do
      hasLettersThatAppearTwice "abcdef" `shouldBe` False
      hasLettersThatAppearTwice "bababc" `shouldBe` True
      hasLettersThatAppearTwice "abbcde" `shouldBe` True
      hasLettersThatAppearTwice "abcccd" `shouldBe` False
      hasLettersThatAppearTwice "aabcdd" `shouldBe` True 
      hasLettersThatAppearTwice "abcdee" `shouldBe` True
      hasLettersThatAppearTwice "ababab" `shouldBe` False

  describe "hasLettersThatAppearThrice" $ do
    it "should return the correct value as per the spec" $ do
      hasLettersThatAppearThrice "abcdef" `shouldBe` False
      hasLettersThatAppearThrice "bababc" `shouldBe` True
      hasLettersThatAppearThrice "abbcde" `shouldBe` False
      hasLettersThatAppearThrice "abcccd" `shouldBe` True 
      hasLettersThatAppearThrice "aabcdd" `shouldBe` False
      hasLettersThatAppearThrice "abcdee" `shouldBe` False
      hasLettersThatAppearThrice "ababab" `shouldBe` True

  describe "differsByOneLetter" $ do
    it "should correctly determine whether strings differ by one letter" $ do
      differsByOneLetter ("abcdef", "abcdeg") `shouldBe` True
      differsByOneLetter ("abddef", "abcdeg") `shouldBe` False

  describe "commonLetters" $ do
    it "should return the common letters between two strings" $ do
      commonLetters "abcdef" "abcdef" `shouldBe` "abcdef"
      commonLetters "abcdef" "abcghi" `shouldBe` "abc"
      commonLetters "abcdef" "ghijkl" `shouldBe` ""