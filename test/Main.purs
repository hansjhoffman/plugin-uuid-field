module Test.Main where

import Prelude

import Data.Either (fromLeft, fromRight)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (parse, toString)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "parse" do
    it "should handle a valid uuid" do
      let
        actual = parse "95ecc380-afe9-11e4-9b6c-751b66dd541e"
        expected = "95ecc380-afe9-11e4-9b6c-751b66dd541e"
      (fromRight "uh-oh" $ toString <$> actual) `shouldEqual` expected
    it "should handle an invalid uuid (invalid chars)" do
      let
        actual = parse "k5ecc380-afe9-11e4-9b6c-751b66dd541e"
        expected = "Expected at least 1 hexadecimal char"
      (fromLeft "uh-oh" $ toString <$> actual) `shouldEqual` expected
    it "should handle an invalid uuid (too long)" do
      let
        actual = parse ("95ecc380-afe9-11e4-9b6c-751b66dd541e" <> "a")
        expected = "Expected 5th chunk to be 12 hexadecimal chars"
      (fromLeft "uh-oh" $ toString <$> actual) `shouldEqual` expected
    it "should handle an invalid uuid (chunk1 too short)" do
      let
        actual = parse ("0-afe9-11e4-9b6c-751b66dd541e" <> "a")
        expected = "Expected 1st chunk to be 8 hexadecimal chars"
      (fromLeft "uh-oh" $ toString <$> actual) `shouldEqual` expected
    it "should handle an invalid uuid (chunk2 too short)" do
      let
        actual = parse "95ecc380-9-11e4-9b6c-751b66dd541e"
        expected = "Expected 2nd chunk to be 4 hexadecimal chars"
      (fromLeft "uh-oh" $ toString <$> actual) `shouldEqual` expected
    it "should handle an invalid uuid (chunk3 too short)" do
      let
        actual = parse "95ecc380-afe9-4-9b6c-751b66dd541e"
        expected = "Expected 3rd chunk to be 4 hexadecimal chars"
      (fromLeft "uh-oh" $ toString <$> actual) `shouldEqual` expected
    it "should handle an invalid uuid (chunk4 too short)" do
      let
        actual = parse "95ecc380-afe9-11e4-6c-751b66dd541e"
        expected = "Expected 4th chunk to be 4 hexadecimal chars"
      (fromLeft "uh-oh" $ toString <$> actual) `shouldEqual` expected
    it "should handle an invalid uuid (chunk5 too short)" do
      let
        actual = parse "95ecc380-afe9-11e4-9b6c-d541e"
        expected = "Expected 5th chunk to be 12 hexadecimal chars"
      (fromLeft "uh-oh" $ toString <$> actual) `shouldEqual` expected
