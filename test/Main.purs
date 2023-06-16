module Test.Main where

import Prelude

import Data.Either (fromLeft, fromRight)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (parse_, toString_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

-- should use QuickCheck and uuid generator for better coverage
main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "parse_" do
    let
      dummyMsg = "foobar"
    it "should handle valid uuid's" do
      ( fromRight dummyMsg $ toString_
          <$> parse_ "23d57c30-afe7-11e4-ab7d-12e3f512a338"
      ) `shouldEqual` "23d57c30-afe7-11e4-ab7d-12e3f512a338" -- uuid v1
      ( fromRight dummyMsg $ toString_
          <$> parse_ "09bb1d8c-4965-4788-94f7-31b151eaba4e"
      ) `shouldEqual` "09bb1d8c-4965-4788-94f7-31b151eaba4e" -- uuid v4
      ( fromRight dummyMsg $ toString_
          <$> parse_ "00000000-0000-0000-0000-000000000000"
      ) `shouldEqual` "00000000-0000-0000-0000-000000000000" -- nil uuid
    it "should handle invalid uuid's" do
      ( fromLeft dummyMsg $ toString_
          <$> parse_ "k5ecc380-afe9-11e4-9b6c-751b66dd541e"
      ) `shouldEqual` "Expected at least 1 hexadecimal char starting at position 1" -- invalid hexadecimal
      ( fromLeft dummyMsg $ toString_
          <$> parse_ "5ecc380-afe9-11e4-9b6c-751b66dd541e"
      ) `shouldEqual` "Expected 1st chunk to be 8 hexadecimal chars starting at position 1" -- invalid length
      ( fromLeft dummyMsg $ toString_
          <$> parse_ "95ecc380-fe9-11e4-9b6c-751b66dd541e"
      ) `shouldEqual` "Expected 2nd chunk to be 4 hexadecimal chars starting at position 9" -- invalid length
      ( fromLeft dummyMsg $ toString_
          <$> parse_ "95ecc380-afe9-1e4-9b6c-751b66dd541e"
      ) `shouldEqual` "Expected 3rd chunk to be 4 hexadecimal chars starting at position 14" -- invalid length
      ( fromLeft dummyMsg $ toString_
          <$> parse_ "95ecc380-afe9-11e4-b6c-751b66dd541e"
      ) `shouldEqual` "Expected 4th chunk to be 4 hexadecimal chars starting at position 19" -- invalid length
      ( fromLeft dummyMsg $ toString_
          <$> parse_ "95ecc380-afe9-11e4-9b6c-51b66dd541e"
      ) `shouldEqual` "Expected 5th chunk to be 12 hexadecimal chars starting at position 24" -- invalid length
