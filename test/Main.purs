module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Main (UUID(..), parse_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

assertRight :: Either String UUID -> UUID -> Aff Unit
assertRight eitherActual expected =
  case eitherActual of
    Left _ -> Right expected `shouldEqual` eitherActual
    Right _ -> Right expected `shouldEqual` eitherActual

assertLeft :: Either String UUID -> String -> Aff Unit
assertLeft eitherActual expected =
  case eitherActual of
    Left _ -> Left expected `shouldEqual` eitherActual
    Right _ -> Left expected `shouldEqual` eitherActual

-- should use QuickCheck and uuid generator for better coverage
main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "parse_" do
    it "should handle valid uuid's" do
      assertRight (parse_ "23d57c30-afe7-11e4-ab7d-12e3f512a338")
        $ UUID "23d57c30-afe7-11e4-ab7d-12e3f512a338" -- uuid v1
      assertRight (parse_ "09bb1d8c-4965-4788-94f7-31b151eaba4e")
        $ UUID "09bb1d8c-4965-4788-94f7-31b151eaba4e" -- uuid v4
      assertRight (parse_ "00000000-0000-0000-0000-000000000000")
        $ UUID "00000000-0000-0000-0000-000000000000" -- nil uuid
    it "should handle invalid uuid's" do
      assertLeft (parse_ "k5ecc380-afe9-11e4-9b6c-751b66dd541e")
        $ "Expected at least 1 hexadecimal char starting at position 1" -- invalid hexadecimal
      assertLeft (parse_ "5ecc380-afe9-11e4-9b6c-751b66dd541e")
        $ "Expected 8 hexadecimal chars starting at position 1" -- invalid length
      assertLeft (parse_ "95ecc380-fe9-11e4-9b6c-751b66dd541e")
        $ "Expected 4 hexadecimal chars starting at position 9" -- invalid length
      assertLeft (parse_ "95ecc380-afe9-1e4-9b6c-751b66dd541e")
        $ "Expected 4 hexadecimal chars starting at position 14" -- invalid length
      assertLeft (parse_ "95ecc380-afe9-11e4-b6c-751b66dd541e")
        $ "Expected 4 hexadecimal chars starting at position 19" -- invalid length
      assertLeft (parse_ "95ecc380-afe9-11e4-9b6c-51b66dd541e")
        $ "Expected 12 hexadecimal chars starting at position 24" -- invalid length
