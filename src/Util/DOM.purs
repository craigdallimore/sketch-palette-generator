module Util.DOM where

import Color (toHexString)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types ( htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Document (createDocumentFragment, createElement, createTextNode)
import DOM.Node.Element (setAttribute)
import DOM.Node.Node (firstChild, removeChild, appendChild)
import DOM.Node.Types (Document, elementToNode, Node, documentFragmentToNode, textToNode)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Prelude
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

appendListItem :: forall eff. Document
                           -> Node
                           -> Color'
                           -> Eff (dom :: DOM | eff) Node
appendListItem doc fragNode (Color' c) = do

  li <- createElement "li" doc

  let liNode = elementToNode li
      hex    = toHexString c

  textNode <- textToNode <$> createTextNode hex doc

  setAttribute "style" ("background-color:" <> hex <> ";") li

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
