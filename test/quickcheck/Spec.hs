import Lib

import Numeric (showFFloat)
import Test.QuickCheck
import Text.Megaparsec (parseMaybe)

main :: IO ()
main = do
  quickCheck propParseNaturalNumber
  quickCheck propParseRationalNumber

propParseNaturalNumber :: Integer -> Bool
propParseNaturalNumber n
  | n >= 0    = fmap toInteger (parseMaybe natParser (show n)) == Just n
  | otherwise = fmap toInteger (parseMaybe natParser (show $ (-1) * n)) == Just ((-1) * n)

-- | Failuer case
-- propParseNaturalNumber :: Integer -> Bool
-- propParseNaturalNumber n = parseMaybe natParser (show n) == Just n

propParseRationalNumber :: Double -> Bool
propParseRationalNumber n =
  fmap fromRational (parseMaybe rationalParser (showFFloat Nothing n "")) == Just n

-- | Failuer case
-- propParseRationalNumber :: Rational -> Bool
-- propParseRationalNumber n =
--   parseMaybe rationalParser (showFFloat Nothing n' "") == Just n
--   where
--     n' = fromRational n :: Double
