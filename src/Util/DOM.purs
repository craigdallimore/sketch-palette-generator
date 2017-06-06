module Util.DOM where

import Color (toHexString)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Classy.Element (toElement)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument, HTMLAnchorElement, HTMLUListElement)
import DOM.HTML.Window (document)
import DOM.Node.Document (createDocumentFragment, createElement, createTextNode)
import DOM.Node.Element (setAttribute, setClassName)
import DOM.Node.Node (firstChild, removeChild, appendChild)
import DOM.Node.Types (Document, elementToNode, Node, documentFragmentToNode, textToNode)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Prelude (Unit, unit, (<$>), bind, pure, discard, (<<<), (<>), (>>=), ($))
import Global (encodeURIComponent)
import Util.Color (isLight)
import Util.Types (Color'(..))

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
  colorListFragNode <- createColorListFragNode colors
  _ <- appendChild colorListFragNode ulNode
  pure unit
  where
    ulNode = (elementToNode <<< toElement) ul

--------------------------------------------------------------------------------

updateDownloadLink :: forall eff. HTMLAnchorElement
                               -> String
                               -> Eff (dom :: DOM | eff) Unit
updateDownloadLink a sketchPalette = do
  setAttribute "href" href aElement
  setAttribute "download" "custom.sketchpalette" aElement
  pure unit
    where
      href     = "data:text/json;charset=utf-8," <> encodeURIComponent sketchPalette
      aElement = toElement a

--------------------------------------------------------------------------------

appendListItem :: forall eff. Document
                           -> Node
                           -> Color'
                           -> Eff (dom :: DOM | eff) Node
appendListItem doc fragNode (Color' c) = do

  li <- createElement "li" doc

  let liNode    = elementToNode li
      hex       = toHexString c
      className = if   isLight c
                  then "preview__item preview__item--light"
                  else "preview__item preview__item--dark"

  textNode <- textToNode <$> createTextNode hex doc

  setAttribute "style" ("background-color:" <> hex <> ";") li
  setClassName className li

  _ <- appendChild textNode liNode
  _ <- appendChild liNode fragNode

  pure fragNode

--------------------------------------------------------------------------------

createColorListFragNode :: forall eff. Array Color'
                        -> Eff (dom :: DOM | eff) Node
createColorListFragNode colors = do

  doc      <- htmlDocumentToDocument <$> (window >>= document)
  fragNode <- documentFragmentToNode <$> createDocumentFragment doc
  _        <- for colors $ appendListItem doc fragNode

  pure fragNode

--------------------------------------------------------------------------------
