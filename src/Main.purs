module Main where

import Control.Applicative (pure)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.Classy.Node (fromNode)
import DOM.Classy.Element (fromElement, toElement)
import DOM.Event.Event (target)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (Event)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (keyup)
import DOM.HTML.HTMLTextAreaElement (value)
import DOM.HTML.Types ( HTMLTextAreaElement
                      , HTMLUListElement
                      , htmlDocumentToParentNode
                      )
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector, QuerySelector(QuerySelector))
import DOM.Node.Types (elementToEventTarget, ParentNode)
import Data.Maybe (Maybe(..), maybe)
import Prelude (Unit, bind, show, (<<<), (>>=), ($), (<$>), (<*>))
import Util.Parse (parse)
import Data.Argonaut (encodeJson)

data Nodes = Nodes HTMLTextAreaElement HTMLUListElement

f :: String -> forall eff. Eff (console :: CONSOLE | eff) Unit
f s = log j where
  j = (show <<< encodeJson <<< parse) s

onTextChange :: forall eff. HTMLUListElement
                         -> Event
                         -> Eff (dom :: DOM, console :: CONSOLE | eff) Unit
onTextChange ul e = maybe
  (log "No textarea node found")
  (\textarea -> value (textarea :: HTMLTextAreaElement) >>= f)
  ((fromNode <<< target) e)

queryNodes :: forall eff. ParentNode -> Eff (dom :: DOM | eff) (Maybe Nodes)
queryNodes docNode = do

  mUlElement       <- querySelector (QuerySelector "#out") docNode
  mTextareaElement <- querySelector (QuerySelector "#in")  docNode

  let mUl       = mUlElement       >>= fromElement
      mTextarea = mTextareaElement >>= fromElement

  pure $ Nodes <$> mTextarea <*> mUl

main :: forall eff. Eff (dom :: DOM, console :: CONSOLE | eff) Unit
main = do
  doc <- window >>= document
  let docNode = htmlDocumentToParentNode doc

  nodes <- queryNodes docNode

  case nodes of

    Nothing        -> log "Could not access elements"
    Just (Nodes textarea ul) -> do

      let listener    = eventListener (onTextChange ul)
          eventTarget = elementToEventTarget (toElement textarea)
      addEventListener keyup listener true eventTarget

--------------------------------------------------------------------------------
