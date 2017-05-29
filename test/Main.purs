module Test.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff.AVar (AVAR)
import Prelude
import Test.Unit (suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Color (rgba)

import Util.Parse (parse)

main :: forall e. Eff (avar :: AVAR, testOutput :: TESTOUTPUT, console :: CONSOLE | e) Unit
main = runTest do
  suite "Parser" do
    test "HEX" do

      assert "an empty string should be []" $
        (parse "") == []

      assert "Six hex digits should parse as a single color"  $
        (parse "000000") == [rgba 0 0 0 1.0]

      assert "Three hex digits should parse to a single color" $
        (parse "000") == [rgba 0 0 0 1.0]

      assert "Six hex digits preceded by # should parse to a single color" $
        (parse "#000000") == [rgba 0 0 0 1.0]

      assert "Three hex digits preceded by # should parse to a single color" $
        (parse "#000") == [rgba 0 0 0 1.0]

      assert "A hex with out of bounds values shall not parse" $
        (parse "#GGG") == []
