module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Main (parse, UUID)

main :: Effect Unit
main = do
  case parse "95ecc380-afe9-11e4-9b6c-751b66dd541e" of
    Right uuid -> log $ show uuid
    Left _ -> log "shit"
