module Main (parse, UUID) where

import Prelude

import Data.CodePoint.Unicode (isHexDigit)
import Data.Either (Either)
import Data.String as Str
import Parsing (Parser, ParseError)
import Parsing as Parsing
import Parsing.Combinators ((<?>))
import Parsing.String (char, eof)
import Parsing.String.Basic (takeWhile1)
import Control.Alt ((<|>))

newtype UUID = UUID String

instance Show UUID where
  show (UUID val) = "(UUID '" <> val <> "')"
  -- show (UUID val) = "(UUID 'uuid:" <> "v1" <> ":" <> val <> "')" -- uuid:v1:95ecc380-afe9-11e4-9b6c-751b66dd541e

-- | Parse a possible uuid string.
parse :: String -> Either ParseError UUID
parse input = do
  Parsing.runParser input parser

-- | INTERNAL
-- |
-- | A parser for uuid's according to the spec:
-- | https://www.ietf.org/rfc/rfc4122.txt
parser :: Parser String UUID
parser = do
  chunk1 <- takeWhile1 isHexDigit -- time_low
  _ <- char '-'
  chunk2 <- takeWhile1 isHexDigit -- time_mid
  _ <- char '-'
  chunk3 <- takeWhile1 isHexDigit -- time_hi_and_version
  _ <- char '-'
  chunk4 <- takeWhile1 isHexDigit -- clock_seq
  _ <- char '-'
  chunk5 <- takeWhile1 isHexDigit -- node
  eof <?> "end of string"

  if (Str.length chunk1 /= 8) then
    Parsing.fail "Expected chunk1 to be 8 hexadecimal digits"
  else if (Str.length chunk2 /= 4) then
    Parsing.fail "Expected chunk2 to be 4 hexadecimal digits"
  else if (Str.length chunk3 /= 4) then
    Parsing.fail "Expected chunk3 to be 4 hexadecimal digits"
  else if (Str.length chunk4 /= 4) then
    Parsing.fail "Expected chunk4 to be 4 hexadecimal digits"
  else if (Str.length chunk5 /= 12) then
    Parsing.fail "Expected chunk5 to be 12 hexadecimal digits"
  else
    pure $ UUID (chunk1 <> "-" <> chunk2 <> "-" <> chunk3 <> "-" <> chunk4 <> "-" <> chunk5)
