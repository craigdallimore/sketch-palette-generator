module Util.Parse where

import Control.Alt ((<|>))
import Data.Array (fromFoldable)
import Data.Char (toLower, toCharCode)
import Data.List (List)
import Data.Either (either)
import Data.String (fromCharArray)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Control.Applicative (pure, (*>))
import Color (fromHexString)
import Text.Parsing.StringParser (Parser, try, fail, runParser)
import Text.Parsing.StringParser.String (char, satisfy, skipSpaces, whiteSpace)
import Text.Parsing.StringParser.Combinators (optional, sepEndBy)
import Prelude ((<$>), (>=), (<=), (&&), (<<<), (||), bind, const)
import Util.Types

isHex :: Char -> Boolean
isHex c = isDigit || isABCDEF where
  isDigit  = code >= 48 && code <= 57  {- 0, 9 -}
  isABCDEF = code >= 97 && code <= 102 {- a, f -}
  code     = (toCharCode <<< toLower) c

hex :: Parser Char
hex = satisfy isHex

parseHex3 :: Parser Color'
parseHex3 = do
  _ <- optional (char '#')
  r <- hex
  g <- hex
  b <- hex
  case fromHexString ("#" <> fromCharArray [r, g, b]) of
    Nothing -> fail "Could not parse HEX3"
    Just c  -> pure (Color' c)

parseHex6 :: Parser Color'
parseHex6 = do
  _  <- optional (char '#')
  r  <- hex
  r' <- hex
  g  <- hex
  g' <- hex
  b  <- hex
  b' <- hex
  case fromHexString ("#" <> fromCharArray [r, r', g, g', b, b']) of
    Nothing -> fail "Could not parse HEX6"
    Just c  -> pure (Color' c)

parseHex :: Parser Color'
parseHex = try parseHex6 <|> parseHex3

parseColor :: Parser (List Color')
parseColor = skipSpaces *> sepEndBy parseHex whiteSpace

parse' :: Parser (List Color') -> String -> Colors'
parse' p s = either
  (const (Colors' []))
  (Colors' <$> fromFoldable)
  (runParser p s)

parse :: String -> Colors'
parse = parse' parseColor
