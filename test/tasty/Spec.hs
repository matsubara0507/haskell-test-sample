import Lib

import Data.Ratio (numerator, denominator)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
  testGroup "Tests" [unitTests, properties]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "zero" $
      calculate "0" @?= Right 0
  , testCase "positive number" $
      calculate "0123456789" @?= Right 123456789
  , testCase "negative number" $
      calculate "-123" @?= Right (-123)
  , testCase "fraction number" $
      calculate "123.0456" @?= Right 123.0456
  , testCase "precedence of operators" $
      calculate "1 + 2 * (3 - 4)" @?= Right (-1)
  ]

properties :: TestTree
properties = testGroup "Properties (checked by QuickCheck)"
  [ testProperty "calculate (show n) == Right n" $
      \n -> (calculate $ show' n) == Right n
  , testProperty "calculate (show n ++ '+' ++ show m) == Right (n + m)" $
      \(n, m) -> calculate (show' n ++ "+" ++ show' m) == Right (n + m)
  , testProperty "calculate (show n ++ '-' ++ show m) == Right (n - m)" $
      \(n, m) -> calculate (show' n ++ "-" ++ show' m) == Right (n - m)
  , testProperty "calculate (show n ++ '*' ++ show m) == Right (n * m)" $
      \(n, m) -> calculate (show' n ++ "*" ++ show' m) == Right (n * m)
  , testProperty "calculate (show n ++ '/' ++ show m) == Right (n / m)" $
      \(n, m) ->
        if m /= 0 then
          calculate (show' n ++ "/" ++ show' m) == Right (n / m)
        else
          True
  ]
  where
    show' :: Rational -> String
    show' n = "(" ++ show (numerator n) ++ "/" ++ show (denominator n) ++ ")"
