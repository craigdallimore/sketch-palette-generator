module Util.Parse where

import Control.Alt ((<|>))
import Data.Array (fromFoldable)
import Data.Char (toLower, toCharCode)
import Data.Either (either)
import Data.String (fromCharArray)
import Data.Maybe (Maybe(..))
import Color (Color, fromHexString)
import Text.Parsing.StringParser (Parser, try, fail, runParser)
import Text.Parsing.StringParser.String (satisfy, char)
import Text.Parsing.StringParser.Combinators (optional, many)
import Prelude

isHex :: Char -> Boolean
isHex c = isDigit || isABCDEF where
  isDigit  = code >= 48 && code <= 57  {- 0, 9 -}
  isABCDEF = code >= 97 && code <= 102 {- a, f -}
  code     = (toCharCode <<< toLower) c

parseHex3 :: Parser Color
parseHex3 = do
  _ <- optional (char '#')
  r <- satisfy isHex
  g <- satisfy isHex
  b <- satisfy isHex
  case (fromHexString ("#" <> fromCharArray [r, g, b])) of
    Nothing -> (fail "Could not parse HEX3")
    Just c  -> pure c

parseHex6 :: Parser Color
parseHex6 = do
  _  <- optional (char '#')
  r  <- satisfy isHex
  r' <- satisfy isHex
  g  <- satisfy isHex
  g' <- satisfy isHex
  b  <- satisfy isHex
  b' <- satisfy isHex
  case (fromHexString ("#" <> fromCharArray [r, r', g, g', b, b'])) of
    Nothing -> (fail "Could not parse HEX6")
    Just c  -> pure c

parseHex :: Parser Color
parseHex = try parseHex6 <|> try parseHex3

parse :: String -> Array Color
parse s = either
  (const [])
  fromFoldable
  (runParser (many parseHex) s)
