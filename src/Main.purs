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
import Prelude

f :: forall eff. Event -> Eff (dom :: DOM, console :: CONSOLE | eff) Unit
f e = maybe
  (log "No node found")
  (\textarea -> value (textarea :: HTMLTextAreaElement) >>= log)
  ((fromNode <<< target) e)

{-
hex  #000000 - #ffffff
rgba 255 255 255 1 - 0 0 0 0
hsla (0/360), 0% - 100%, 0% - 100%, 0 -1



-}

main :: forall eff. Eff (dom :: DOM, console :: CONSOLE | eff) Unit
main = do
  doc <- window >>= document
  el  <- querySelector (QuerySelector "#in") (htmlDocumentToParentNode doc)
  maybe
    (log "No element #in found")
    (addEventListener keyup (eventListener f) true)
    (elementToEventTarget <$> el)
