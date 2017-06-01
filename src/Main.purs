module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.Classy.Node (fromNode)
import DOM.Event.Event (target)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (Event)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (keyup)
import DOM.HTML.HTMLTextAreaElement (value)
import DOM.HTML.Types (HTMLTextAreaElement, htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector, QuerySelector(QuerySelector))
import DOM.Node.Types (elementToEventTarget)
import Data.Maybe (maybe)
import Prelude (Unit, bind, show, (<$>), (<<<), (>>=))
import Util.Parse (parse)
import Data.Argonaut (encodeJson)

f :: String -> forall eff. Eff (console :: CONSOLE | eff) Unit
f s = log j where
  j = (show <<< encodeJson <<< parse) s

onTextChange :: forall eff. Event -> Eff (dom :: DOM, console :: CONSOLE | eff) Unit
onTextChange e = maybe
  (log "No textarea node found")
  (\textarea -> value (textarea :: HTMLTextAreaElement) >>= f)
  ((fromNode <<< target) e)

main :: forall eff. Eff (dom :: DOM, console :: CONSOLE | eff) Unit
main = do
  doc <- window >>= document
  el  <- querySelector (QuerySelector "#in") (htmlDocumentToParentNode doc)
  maybe
    (log "No element #in found")
    (addEventListener keyup (eventListener onTextChange) true)
    (elementToEventTarget <$> el)
