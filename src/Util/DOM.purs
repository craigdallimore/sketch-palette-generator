module Util.DOM where

import Color (toHexString, darken)
import Control.Monad.Eff (Eff)
import Data.Array (length)
import DOM (DOM)
import DOM.Classy.Element (toElement)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument, HTMLAnchorElement, HTMLUListElement)
import DOM.HTML.Window (document)
import DOM.Node.Document (createDocumentFragment, createElement)
import DOM.Node.Element (setAttribute, removeAttribute, setClassName)
import DOM.Node.Node (firstChild, removeChild, appendChild, setTextContent)
import DOM.Node.Types (Document, elementToNode, Node, documentFragmentToNode)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Data.Argonaut (encodeJson)
import Prelude (Unit, unit, show, (<$>), bind, pure, discard, (<<<), (<>), (>>=), ($), (==))
import Global (encodeURIComponent)
import Util.Color (isLight)
import Util.Types (Color'(..))

--------------------------------------------------------------------------------

isEmpty :: forall a. Array a -> Boolean
isEmpty xs = length xs == 0

--------------------------------------------------------------------------------

removeChildren :: forall eff. Node -> Eff (dom :: DOM | eff) Node
removeChildren parentNode = do
  maybeFirstChild <- firstChild parentNode
  case maybeFirstChild of
    Nothing        -> pure parentNode
    Just childNode -> do
      _ <- removeChild childNode parentNode
      removeChildren parentNode

--------------------------------------------------------------------------------

updatePalette :: forall eff. HTMLUListElement
                          -> Array Color'
                          -> Eff (dom :: DOM | eff) Unit
updatePalette ul colors = do
  _ <- removeChildren ulNode

  if isEmpty colors then do
      doc <- htmlDocumentToDocument <$> (window >>= document)
      li  <- createElement "li" doc

      let liNode = elementToNode li

      setClassName "preview__item preview__item--empty" li
      setTextContent "Maybe try one hexcode per line?" liNode

      _ <- appendChild liNode ulNode

      pure unit

    else do
      colorListFragNode <- createColorListFragNode colors
      _ <- appendChild colorListFragNode ulNode
      pure unit

  where
    ulNode = (elementToNode <<< toElement) ul

--------------------------------------------------------------------------------

updateDownloadLink :: forall eff. HTMLAnchorElement
                               -> Array Color'
                               -> Eff (dom :: DOM | eff) Unit
updateDownloadLink a colors = if isEmpty colors
  then do
    setAttribute "disabled" "disabled" aElement
    pure unit
  else do
    removeAttribute "disabled" aElement
    setAttribute "href" href aElement
    setAttribute "download" "custom.sketchpalette" aElement
    pure unit
  where
    href     = "data:text/json;charset=utf-8," <> encodeURIComponent sketchPalette
    aElement = toElement a
    sketchPalette = (show <<< encodeJson) colors

--------------------------------------------------------------------------------

appendPreviewSwatchItem :: forall eff. Document
                                    -> Node
                                    -> Color'
                                    -> Eff (dom :: DOM | eff) Node
appendPreviewSwatchItem doc fragNode (Color' c) = do

  li <- createElement "li" doc

  let liNode    = elementToNode li
      bgHex     = toHexString c
      bdrHex    = (toHexString <<< darken 0.1) c
      style     = "background-color:" <> bgHex <> ";" <> "border-color:" <> bdrHex <> ";"
      className = if isLight c
                  then "preview__item preview__item--light"
                  else "preview__item preview__item--dark"

  setAttribute "style" style li
  setAttribute "title" bgHex li
  setClassName className li

  _ <- appendChild liNode fragNode

  pure fragNode

--------------------------------------------------------------------------------

createColorListFragNode :: forall eff. Array Color'
                        -> Eff (dom :: DOM | eff) Node
createColorListFragNode colors = do

  doc      <- htmlDocumentToDocument <$> (window >>= document)
  fragNode <- documentFragmentToNode <$> createDocumentFragment doc
  _        <- for colors $ appendPreviewSwatchItem doc fragNode

  pure fragNode

--------------------------------------------------------------------------------
