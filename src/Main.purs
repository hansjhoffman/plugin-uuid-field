module Main
  ( UUID(..)
  , format_
  , parse_
  , toString_
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.CodePoint.Unicode (isHexDigit)
import Data.Either (Either)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Show.Generic (genericShow)
import Data.String as String
import Parsing (ParseError, Parser, Position(..))
import Parsing as Parsing
import Parsing.Combinators ((<?>))
import Parsing.String (char, eof)
import Parsing.String.Basic (takeWhile1)

newtype UUID = UUID String

derive instance newtypeUUID :: Newtype UUID _

derive instance eqUUID :: Eq UUID

derive instance genericUUID :: Generic UUID _

instance showUUID :: Show UUID where
  show = genericShow

-- | INTERNAL
-- |
-- | A parser for uuid's according to the spec:
-- | https://www.ietf.org/rfc/rfc4122.txt
parser :: Parser String UUID
parser = do
  chunk1 <- takeWhile1 isHexDigit <?> "at least 1 hexadecimal char"
  _ <- char '-'
  chunk2 <- takeWhile1 isHexDigit <?> "at least 1 hexadecimal char"
  _ <- char '-'
  chunk3 <- takeWhile1 isHexDigit <?> "at least 1 hexadecimal char"
  _ <- char '-'
  chunk4 <- takeWhile1 isHexDigit <?> "at least 1 hexadecimal char"
  _ <- char '-'
  chunk5 <- takeWhile1 isHexDigit <?> "at least 1 hexadecimal char"
  eof <?> "end of string"

  if (String.length chunk1 /= 8) then
    Parsing.failWithPosition "Expected 8 hexadecimal chars" $
      Position { column: 1, index: 0, line: 1 }
  else if (String.length chunk2 /= 4) then
    Parsing.failWithPosition "Expected 4 hexadecimal chars" $
      Position { column: 9, index: 8, line: 1 }
  else if (String.length chunk3 /= 4) then
    Parsing.failWithPosition "Expected 4 hexadecimal chars" $
      Position { column: 14, index: 13, line: 1 }
  else if (String.length chunk4 /= 4) then
    Parsing.failWithPosition "Expected 4 hexadecimal chars" $
      Position { column: 19, index: 18, line: 1 }
  else if (String.length chunk5 /= 12) then
    Parsing.failWithPosition "Expected 12 hexadecimal chars" $
      Position { column: 24, index: 23, line: 1 }
  else
    pure $ UUID
      ( intercalate "-"
          [ chunk1
          , chunk2
          , chunk3
          , chunk4
          , chunk5
          ]
      )

-- | INTERNAL
prettyError :: ParseError -> String
prettyError err = msg <> " starting at position " <> show col
  where
  msg = Parsing.parseErrorMessage err
  Position { column: col, index: _, line: _ } = Parsing.parseErrorPosition err

-- | Parse a string as a possible uuid.
parse_ :: String -> Either String UUID
parse_ = lmap prettyError
  <<< flip Parsing.runParser parser
  <<< String.trim

-- | Unwraps a UUID type
toString_ :: UUID -> String
toString_ = unwrap

-- | Pretty formats a UUID (opinionated).
format_ :: UUID -> UUID
format_ = over UUID String.toLower
