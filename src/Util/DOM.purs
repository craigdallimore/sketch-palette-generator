module Util.DOM where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Node.Node (firstChild, removeChild, appendChild)
import DOM.HTML.Types ( htmlDocumentToDocument)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.Node.Document (createDocumentFragment, createElement)
import DOM.Node.Element (setAttribute)
import DOM.Node.Types (elementToNode, Node, DocumentFragment, documentFragmentToNode)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Prelude
import Util.Types (Color')

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

createColorListFrag :: forall eff. Array Color' -> Eff (dom :: DOM | eff) DocumentFragment
createColorListFrag colors = do
  doc <- htmlDocumentToDocument <$> (window >>= document)
  frag <- createDocumentFragment doc
  let fragNode = documentFragmentToNode frag
  _ <- for colors $ \color -> do
    li <- createElement "li" doc
    setAttribute "style" ("background-color:" <> (show color) <> ";") li
    appendChild (elementToNode li) fragNode
  pure frag

--------------------------------------------------------------------------------
