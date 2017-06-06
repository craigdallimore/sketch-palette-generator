module Util.Types where

import Prelude ((<$>), div)
import Data.Int (toNumber)
import Data.Show (class Show)
import Data.Ord (class Ord, compare)
import Data.Eq (class Eq, eq)

import Data.Argonaut hiding (toNumber)
import Color (Color, toRGBA, toHSLA, toHexString)

newtype Color' = Color' Color
newtype Colors' = Colors' (Array Color')

instance eqColor :: Eq Color' where
  eq (Color' c1) (Color' c2) = eq c1 c2

instance ordColor :: Ord Color' where
  compare (Color' c1) (Color' c2) = compare hsla1.h hsla2.h where
    hsla1 = toHSLA c1
    hsla2 = toHSLA c2

instance showColor :: Show Color' where
  show (Color' color) = toHexString color

instance encodeJsonColor :: EncodeJson Color' where
  encodeJson (Color' color)
     = "red"   := div (toNumber rgba.r) 255.0
    ~> "green" := div (toNumber rgba.g) 255.0
    ~> "blue"  := div (toNumber rgba.b) 255.0
    ~> "alpha" := rgba.a
    ~> jsonEmptyObject
      where rgba = toRGBA color

instance encodeJsonColors :: EncodeJson Colors' where
  encodeJson (Colors' colors)
     = "compatibleVersion" := "1.4"
    ~> "pluginVersion"     := "1.4"
    ~> "colors"            := fromArray (encodeJson <$> colors)
    ~> jsonEmptyObject
