module Main
  ( UUID
  , format
  , parse
  , toString
  ) where

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

instance showUUID :: Show UUID where
  show (UUID uuid) = "(UUID " <> uuid <> ")"

instance eqUUID :: Eq UUID where
  eq (UUID x) (UUID y) = x == y

-- | Parse a possible uuid string.
parse :: String -> Either String UUID
parse = lmap Parsing.parseErrorMessage <<< flip Parsing.runParser parser

-- | Converts a parsed UUID into a string.
toString :: UUID -> String
toString (UUID uuid) = uuid

-- | Pretty formats a UUID (opinionated).
format :: UUID -> UUID
format (UUID uuid) = UUID $ Str.toLower uuid

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
