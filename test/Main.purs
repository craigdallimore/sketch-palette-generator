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

c0 = rgba  0  0  0 1.0 :: Color
c1 = rgba 17 17 17 1.0 :: Color
c2 = rgba 34 34 34 1.0 :: Color
c3 = rgba 51 51 51 1.0 :: Color

css :: String
css = ".selector {\n\t\n\t  border : 1px solid #000;\n\t  color: #111;\n\t  background: #222;\n\t  /* even#333*/\n\t\n\t\n\t}\n"

main :: forall e. Eff (avar :: AVAR, testOutput :: TESTOUTPUT, console :: CONSOLE | e) Unit
main = runTest do
  suite "Parser" do
    test "HEX" do

      assert "an empty string should be []" $
        (parse "") == []

      assert "Six hex digits should parse as a single color"  $
        (parse "000000") == [c0]

      assert "Three hex digits should parse to a single color" $
        (parse "000") == [c0]

      assert "Six hex digits preceded by # should parse to a single color" $
        (parse "#000000") == [c0]

      assert "Three hex digits preceded by # should parse to a single color" $
        (parse "#000") == [c0]

      assert "A hex with out of bounds values shall not parse" $
        (parse "#GGG") == []

      assert "Multiple hexes on one trim line shall be parsed" $
        (parse "#000 #111 #222") == [c0, c1, c2]

      assert "Multiple hexes on one line shall be parsed" $
        (parse " #000 #111 #222 ") == [c0, c1, c2]

      assert "Multiple hexes on multiple lines shall be parsed" $
        (parse " #000 #111\n #222 #333") == [c0, c1, c2, c3]

      assert "Hexes can be parsed from CSS" $
        (parse css) == [c0, c1, c2, c3]

