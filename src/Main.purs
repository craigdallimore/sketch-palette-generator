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

-- Problems encountered
-- 1. What do I need?
-- 2. How do I access document.querySelector
-- 3. how do I access document?
-- 4. how do I create a QuerySelector ... oh, and it needs to be wrapped
-- 5. the document is not a parentNode
-- 6. how do I make an event listener
-- 7. the element is a maybe element
-- 8. the element is not an event target
-- 9. How can I access e.target.value?

f :: forall eff. Event -> Eff (dom :: DOM, console :: CONSOLE | eff) Unit
f e = maybe
  (log "No node found")
  (\textarea -> value (textarea :: HTMLTextAreaElement) >>= log)
  ((fromNode <<< target) e)

main :: forall eff. Eff (dom :: DOM, console :: CONSOLE | eff) Unit
main = do
  doc <- window >>= document
  el  <- querySelector (QuerySelector "#in") (htmlDocumentToParentNode doc)
  maybe
    (log "No element #in found")
    (addEventListener keyup (eventListener f) true)
    (elementToEventTarget <$> el)
