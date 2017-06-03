module Main where

import Control.Applicative (pure)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.Classy.Node (fromNode)
import DOM.Classy.Element (fromElement, toElement)
import DOM.Classy.ParentNode (toParentNode)
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
import DOM.Node.ParentNode (querySelector, QuerySelector(QuerySelector), children)
import DOM.Node.Types (elementToEventTarget, ParentNode)
import Data.Maybe (Maybe, maybe)
import Prelude (Unit, bind, show, (<<<), (>>=), ($), (<$>), (<*>), (=<<))
import Util.Parse (parse)
import Data.Argonaut (encodeJson)

--------------------------------------------------------------------------------

data Nodes = Nodes HTMLTextAreaElement HTMLUListElement

--------------------------------------------------------------------------------

updateDOM :: HTMLUListElement
          -> String
          -> forall eff. Eff (dom :: DOM, console :: CONSOLE | eff) Unit
updateDOM ul input = do
  lis <- children (toParentNode ul)
  log sketchPalette

    where
      sketchPalette = (show <<< encodeJson <<< parse) input

--------------------------------------------------------------------------------

onTextChange :: forall eff. HTMLUListElement
                         -> Event
                         -> Eff (dom :: DOM, console :: CONSOLE | eff) Unit
onTextChange ul e = maybe
  (log "No textarea node found")
  (\textarea -> value (textarea :: HTMLTextAreaElement) >>= updateDOM ul)
  ((fromNode <<< target) e)

--------------------------------------------------------------------------------

queryNodes :: forall eff. ParentNode -> Eff (dom :: DOM | eff) (Maybe Nodes)
queryNodes docNode = do

  -- querySelector qs docNode :: Eff ( dom ∷ DOM | eff ) (Maybe Element)
  -- (=<<) fromElement        :: (Element -> Maybe SpecialisedElement)
  --                             -> Maybe Element
  --                             -> Maybe SpecialisedElement
  --
  -- Thus we lift that into Eff so that we can extract a Maybe SpecialisedElement

  textarea <- (=<<) fromElement <$> querySelector (QuerySelector "#in")  docNode
  ul       <- (=<<) fromElement <$> querySelector (QuerySelector "#out") docNode

  pure $ Nodes <$> textarea <*> ul

--------------------------------------------------------------------------------

bindDOM :: forall eff. Nodes -> Eff (dom :: DOM, console :: CONSOLE | eff) Unit
bindDOM (Nodes textarea ul) = do
  let listener    = eventListener (onTextChange ul)
      eventTarget = elementToEventTarget (toElement textarea)
  addEventListener keyup listener true eventTarget

--------------------------------------------------------------------------------

main :: forall eff. Eff (dom :: DOM, console :: CONSOLE | eff) Unit
main = window
  >>= document
  >>= (queryNodes <<< htmlDocumentToParentNode)
  >>= (maybe (log "Could not access elements") bindDOM)

--------------------------------------------------------------------------------
