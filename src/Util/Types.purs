module Util.Types where

import Prelude ((<$>), div)
import Data.Int (toNumber)

import Data.Argonaut hiding (toNumber)
import Color (Color, toRGBA)

newtype Color' = Color' Color
newtype Colors' = Colors' (Array Color')

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
