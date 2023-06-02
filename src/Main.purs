module Main (parse, UUID(..)) where

import Prelude

import Data.CodePoint.Unicode as Unicode
import Data.Either (Either)
import Data.String as Str
import Parsing (Parser, ParseError)
import Parsing as Parsing
import Parsing.Combinators ((<?>))
import Parsing.String as Parsing.String
import Parsing.String.Basic as String.Basic
import Control.Alt ((<|>))

newtype UUID = UUID String

derive instance eqUUID :: Eq UUID

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
  time_low <- String.Basic.takeWhile1 Unicode.isHexDigit
  _ <- Parsing.String.char '-'
  time_mid <- String.Basic.takeWhile1 Unicode.isHexDigit
  _ <- Parsing.String.char '-'
  time_hi_and_version <- String.Basic.takeWhile1 Unicode.isHexDigit
  _ <- Parsing.String.char '-'
  clock_seq <- String.Basic.takeWhile1 Unicode.isHexDigit
  _ <- Parsing.String.char '-'
  node <- String.Basic.takeWhile1 Unicode.isHexDigit
  Parsing.String.eof <?> "end of string"

  if (Str.length time_low /= 8) then
    Parsing.fail "Expected 1st chunk to be 8 hexadecimal digits"
  else if (Str.length time_mid /= 4) then
    Parsing.fail "Expected 2nd chunk to be 4 hexadecimal digits"
  else if (Str.length time_hi_and_version /= 4) then
    Parsing.fail "Expected 3rd chunk to be 4 hexadecimal digits"
  else if (Str.length clock_seq /= 4) then
    Parsing.fail "Expected 4th chunk to be 4 hexadecimal digits"
  else if (Str.length node /= 12) then
    Parsing.fail "Expected 5th chunk to be 12 hexadecimal digits"
  else
    pure $ UUID (time_low <> "-" <> time_mid <> "-" <> time_hi_and_version <> "-" <> clock_seq <> "-" <> node)
