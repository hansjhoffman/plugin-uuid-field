module Main
  ( UUID
  , format
  , parse_
  , toString
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.CodePoint.Unicode as Unicode
import Data.Either (Either)
import Data.Foldable as Data.Foldable
import Data.String as Data.String
import Parsing (Parser)
import Parsing as Parsing
import Parsing.Combinators ((<?>))
import Parsing.String as Parsing.String
import Parsing.String.Basic as String.Basic

newtype UUID = UUID String

instance showUUID :: Show UUID where
  show (UUID uuid) = "(UUID " <> show uuid <> ")"

instance eqUUID :: Eq UUID where
  eq (UUID uuid1) (UUID uuid2) = uuid1 == uuid2

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

  if (Data.String.length chunk1 /= 8) then
    Parsing.failWithPosition "Expected 1st chunk to be 8 hexadecimal chars" $
      Parsing.Position { column: 1, index: 0, line: 1 }
  else if (Data.String.length chunk2 /= 4) then
    Parsing.failWithPosition "Expected 2nd chunk to be 4 hexadecimal chars" $
      Parsing.Position { column: 9, index: 8, line: 1 }
  else if (Data.String.length chunk3 /= 4) then
    Parsing.failWithPosition "Expected 3rd chunk to be 4 hexadecimal chars" $
      Parsing.Position { column: 14, index: 13, line: 1 }
  else if (Data.String.length chunk4 /= 4) then
    Parsing.failWithPosition "Expected 4th chunk to be 4 hexadecimal chars" $
      Parsing.Position { column: 19, index: 18, line: 1 }
  else if (Data.String.length chunk5 /= 12) then
    Parsing.failWithPosition "Expected 5th chunk to be 12 hexadecimal chars" $
      Parsing.Position { column: 24, index: 23, line: 1 }
  else
    pure $ mkUUID chunk1 chunk2 chunk3 chunk4 chunk5

-- | Internal
mkUUID :: String -> String -> String -> String -> String -> UUID
mkUUID chunk1 chunk2 chunk3 chunk4 chunk5 =
  UUID $ Data.Foldable.intercalate "-" [ chunk1, chunk2, chunk3, chunk4, chunk5 ]

-- | INTERNAL
prettyError :: Parsing.ParseError -> String
prettyError err = msg <> " starting at position " <> show col
  where
  msg = Parsing.parseErrorMessage err
  Parsing.Position { column: col, index: _, line: _ } = Parsing.parseErrorPosition err

-- | Parse a string as a possible uuid.
parse_ :: String -> Either String UUID
parse_ = lmap prettyError
  <<< flip Parsing.runParser parser
  <<< Data.String.trim

-- | Unwrap UUID type
toString :: UUID -> String
toString (UUID uuid) = uuid

-- | Pretty formats a UUID (opinionated).
format :: UUID -> UUID
format = UUID <$> Data.String.toLower <<< toString
