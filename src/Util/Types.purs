module Util.Types where

import Prelude ((<$>))

import Data.Argonaut
import Color (Color, toRGBA)

newtype Color' = Color' Color
newtype Colors' = Colors' (Array Color')

instance encodeJsonColor :: EncodeJson Color' where
  encodeJson (Color' color)
     = "red"   := rgba.r
    ~> "green" := rgba.g
    ~> "blue"  := rgba.b
    ~> "alpha" := rgba.a
    ~> jsonEmptyObject
      where rgba = toRGBA color

instance encodeJsonColors :: EncodeJson Colors' where
  encodeJson (Colors' colors)
     = "compatibleVersion" := "1.4"
    ~> "pluginVersion"     := "1.4"
    ~> "colors"            := fromArray (encodeJson <$> colors)
    ~> jsonEmptyObject
