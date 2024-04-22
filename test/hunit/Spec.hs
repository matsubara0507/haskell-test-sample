import Lib

import Data.Bifunctor (first)
import Data.Ratio ((%))
import Test.HUnit
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = runTestTTAndExit $ TestList
  [ TestLabel "natParser" $ TestList
      [ TestCase $
          parse' natParser "zero number" "0" @?= Right 0
      , TestCase $
          parse' natParser "positive number" "1234567890" @?= Right 1234567890
      , TestCase $
          parse' natParser "negative number" "-1" @?=
            Left "negative number:1:1:\n  |\n1 | -1\n  | ^\nunexpected '-'\nexpecting numeric character\n"
      , TestCase $
          parse' natParser "rational number" "0.0123" @?= Right 0
      ]
  , TestLabel "rationalParser" $ TestList
      [ TestCase $
          parse' rationalParser "zero number" "0" @?= Right (0 % 1)
      , TestCase $
          parse' rationalParser "positive number" "1234567890" @?= Right (1234567890 % 1)
      , TestCase $
          parse' rationalParser "negative number" "-1" @?= Right (-(1 % 1))
      , TestCase $
          parse' rationalParser "rational number" "0.0123" @?= Right (123 % 10000)
      ]
  ]
  where
    parse' p s i = first errorBundlePretty $ parse p s i
