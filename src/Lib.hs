module Lib
    ( calculate
    , rationalParser
    , natParser
    ) where

import Data.Ratio ((%))
import Numeric.Natural (Natural)
import Text.Megaparsec
import Text.Megaparsec.Char (numberChar, char, hspace)
import Text.Read (readMaybe)

type Parser = Parsec CustomError String
type ParserError = ParseErrorBundle String CustomError

newtype CustomError = ReadError String deriving (Eq, Ord)

instance ShowErrorComponent CustomError where
  showErrorComponent (ReadError s) = s

-- | BNF
-- expr     ::= term (('+'|'-') term)*
-- term     ::= factor (('*'|'/') factor)*
-- factor   ::= '(' expr ')' | rational
-- rational ::= ('-'|ε)natural('.'natural|ε)
-- natural  ::= [0-9]+
calculate :: String -> Either ParserError Rational
calculate = parse exprParser "calculate"

exprParser :: Parser Rational
exprParser = do
  t <- termParser
  exprParser' t
  where
    exprParser' t = do
      hspace
      op <- optional (char '+' <|> char '-')
      case op of
        Nothing  -> pure t
        Just op' -> do
          hspace
          t' <- termParser
          if op' == '+' then
            exprParser' (t + t')
          else
            exprParser' (t - t')

termParser :: Parser Rational
termParser = do
  t <- factorParser
  termParser' t
  where
    termParser' t = do
      hspace
      op <- optional (char '*' <|> char '/')
      case op of
        Nothing  -> pure t
        Just op' -> do
          hspace
          t' <- factorParser
          if op' == '*' then
            termParser' (t * t')
          else
            termParser' (t / t')

factorParser :: Parser Rational
factorParser = do
  br <- optional (char '(')
  case br of
    Nothing -> rationalParser
    Just _  -> do
      hspace
      t <- exprParser
      hspace
      _ <- char ')'
      pure t

rationalParser :: Parser Rational
rationalParser = do
  sign <- signParser
  nat  <- toRational <$> natParser
  dot  <- optional (char '.')
  case dot of
    Just _  -> (* sign) <$> (+ nat) <$> fracParser
    Nothing -> pure $ sign * nat

signParser :: Parser Rational
signParser = maybe 1 (const (-1)) <$> optional (char '-')

natParser :: Parser Natural
natParser = do
  nat <- some numberChar
  case readMaybe nat of
    Nothing -> customFailure (ReadError nat)
    Just n  -> pure n

fracParser :: Parser Rational
fracParser = do
  nat <- some numberChar
  case readMaybe nat of
    Nothing -> customFailure (ReadError nat)
    Just n  -> pure $ n % (10 ^ (length nat))
