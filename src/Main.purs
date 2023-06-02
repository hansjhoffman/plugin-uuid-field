module Main (parse, UUID(..)) where

import Prelude

import Data.Bifunctor (lmap)
import Data.CodePoint.Unicode as Unicode
import Data.Either (Either)
import Data.String as Str
import Parsing (Parser)
import Parsing as Parsing
import Parsing.Combinators ((<?>))
import Parsing.String as Parsing.String
import Parsing.String.Basic as String.Basic

newtype UUID = UUID String

derive instance eqUUID :: Eq UUID

instance Show UUID where
  show (UUID val) = "(UUID '" <> val <> "')"

-- show (UUID val) = "(UUID 'uuid:" <> "v1" <> ":" <> val <> "')" -- uuid:v1:95ecc380-afe9-11e4-9b6c-751b66dd541e

-- | Parse a possible uuid string.
parse :: String -> Either String UUID
parse = lmap Parsing.parseErrorMessage <<< flip Parsing.runParser parser

-- | INTERNAL
-- |
-- | A parser for uuid's according to the spec:
-- | https://www.ietf.org/rfc/rfc4122.txt
parser :: Parser String UUID
parser = do
  chunk1 <-
    String.Basic.takeWhile1 Unicode.isHexDigit
      <?> "at least 1 hexadecimal char"
  _ <- Parsing.String.char '-'
  chunk2 <-
    String.Basic.takeWhile1 Unicode.isHexDigit
      <?> "at least 1 hexadecimal char"
  _ <- Parsing.String.char '-'
  chunk3 <-
    String.Basic.takeWhile1 Unicode.isHexDigit
      <?> "at least 1 hexadecimal char"
  _ <- Parsing.String.char '-'
  chunk4 <-
    String.Basic.takeWhile1 Unicode.isHexDigit
      <?> "at least 1 hexadecimal char"
  _ <- Parsing.String.char '-'
  chunk5 <-
    String.Basic.takeWhile1 Unicode.isHexDigit
      <?> "at least 1 hexadecimal char"
  Parsing.String.eof <?> "end of string"

  if (Str.length chunk1 /= 8) then
    Parsing.fail "Expected 1st chunk to be 8 hexadecimal chars"
  else if (Str.length chunk2 /= 4) then
    Parsing.fail "Expected 2nd chunk to be 4 hexadecimal chars"
  else if (Str.length chunk3 /= 4) then
    Parsing.fail "Expected 3rd chunk to be 4 hexadecimal chars"
  else if (Str.length chunk4 /= 4) then
    Parsing.fail "Expected 4th chunk to be 4 hexadecimal chars"
  else if (Str.length chunk5 /= 12) then
    Parsing.fail "Expected 5th chunk to be 12 hexadecimal chars"
  else
    pure $ UUID
      ( chunk1
          <> "-"
          <> chunk2
          <> "-"
          <> chunk3
          <> "-"
          <> chunk4
          <> "-"
          <> chunk5
      )
