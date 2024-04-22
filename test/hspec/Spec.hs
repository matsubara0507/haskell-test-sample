import Lib

import Data.Bifunctor (first)
import Data.Ratio ((%))
import Numeric (showFFloat)
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec (parse, parseMaybe)
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = hspec $ do
  describe "Lib.natParser" $ do
    let subject n = parse' natParser "" n

    context "when input is 0" $
      it "should be same number" $
        subject "0" `shouldBe` Right 0

    context "when input is positive number" $
      it "should be same number" $
        subject "1234567890" `shouldBe` Right 1234567890

    context "when input is negative number" $
      it "should be error" $
        subject "-1" `shouldBe` Left "1:1:\n  |\n1 | -1\n  | ^\nunexpected '-'\nexpecting numeric character\n"

    context "when input is rational number" $
      it "should be 0" $
        subject "0.0123" `shouldBe` Right 0

    it "return same natural number" $
      property propParseNaturalNumber

  describe "Lib.rationalParser" $ do
    let subject n = parse' rationalParser "" n

    context "when input is 0" $
      it "should be same number" $
        subject "0" `shouldBe` Right 0

    context "when input is positive number" $
      it "should be same number" $
        subject "1234567890" `shouldBe` Right 1234567890

    context "when input is negative number" $
      it "should be same number" $
        subject "-1" `shouldBe` Right (-1)

    context "when input is rational number" $
      it "should be same number" $
        subject "0.0123" `shouldBe` Right (123 % 10000)

    it "return same float number" $
      property propParseRationalNumber

  where
    parse' p s i = first errorBundlePretty $ parse p s i

propParseNaturalNumber :: Integer -> Bool
propParseNaturalNumber n
  | n >= 0    = fmap toInteger (parseMaybe natParser (show n)) == Just n
  | otherwise = fmap toInteger (parseMaybe natParser (show $ (-1) * n)) == Just ((-1) * n)

propParseRationalNumber :: Double -> Bool
propParseRationalNumber n =
  fmap fromRational (parseMaybe rationalParser (showFFloat Nothing n "")) == Just n
