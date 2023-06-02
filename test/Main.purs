module Test.MainSpec where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (parse, UUID(..))
-- import Parsing (ParseError(..))
import Test.Spec (describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "parse" do
    it "should handle a valid uuid" do
      let
        actual = parse "95ecc380-afe9-11e4-9b6c-751b66dd541e"
        expected = Right (UUID "95ecc380-afe9-11e4-9b6c-751b66dd541e")
      actual `shouldEqual` expected

    pending "should handle an invalid uuid (invalid chars)"

    -- it "should handle an invalid uuid (too long)" do
    --   let
    --     actual = parse ("95ecc380-afe9-11e4-9b6c-751b66dd541e" <> "a")
    --     expected = Left (ParseError "8" "Expected 5th chunk to be 12 hexadecimal digits")
    --   actual `shouldEqual` expected

    pending "should handle an invalid uuid (chunk1 too short)"
    pending "should handle an invalid uuid (chunk2 too short)"
    pending "should handle an invalid uuid (chunk3 too short)"
    pending "should handle an invalid uuid (chunk4 too short)"
    pending "should handle an invalid uuid (chunk5 too short)"
