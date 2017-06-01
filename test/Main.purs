module Test.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff.AVar (AVAR)
import Prelude
import Test.Unit (suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Color (Color, rgba)

import Util.Parse (parse)
import Util.Types

c0 = Color' $ rgba  0  0  0 1.0
c1 = Color' $ rgba 17 17 17 1.0
c2 = Color' $ rgba 34 34 34 1.0
c3 = Color' $ rgba 51 51 51 1.0

main :: forall e. Eff (avar :: AVAR, testOutput :: TESTOUTPUT, console :: CONSOLE | e) Unit
main = runTest do
  suite "HEX -> sketch palette" do
    test "Parsing colors from a String" do

      assert "an empty string should be []" $
        (parse "") == Colors' []

      assert "Six hex digits should parse as a single color"  $
        (parse "000000") == Colors' [c0]

      assert "Three hex digits should parse to a single color" $
        (parse "000") == Colors' [c0]

      assert "Six hex digits preceded by # should parse to a single color" $
        (parse "#000000") == Colors' [c0]

      assert "Three hex digits preceded by # should parse to a single color" $
        (parse "#000") == Colors' [c0]

      assert "A hex with out of bounds values shall not parse" $
        (parse "#GGG") == Colors' []

      assert "Multiple hexes on one trim line shall be parsed" $
        (parse "#000 #111 #222") == Colors' [c0, c1, c2]

      assert "Multiple hexes on one line shall be parsed" $
        (parse " #000 #111 #222 ") == Colors' [c0, c1, c2]

      assert "Multiple hexes on multiple lines shall be parsed" $
        (parse " #000 #111\n #222 #333") == Colors' [c0, c1, c2, c3]
