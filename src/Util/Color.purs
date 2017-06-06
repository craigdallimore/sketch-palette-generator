module Util.Color where

import Color (Color, toRGBA)
import Data.Int (toNumber)
import Prelude

--------------------------------------------------------------------------------

isLight :: Color -> Boolean
isLight c = perceptiveLuminance < 0.5 where

  -- Reference / credit: https://stackoverflow.com/questions/1855884/determine-font-color-based-on-background-color
  perceptiveLuminance = 1.0 - (0.299 * r + 0.587 * g + 0.114 * b) / 255.0

  rgba = toRGBA   c
  r    = toNumber rgba.r
  g    = toNumber rgba.g
  b    = toNumber rgba.b

--------------------------------------------------------------------------------
