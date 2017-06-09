module Main where

import Control.Applicative (pure)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.Classy.Element (fromElement, toElement)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (Event)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (keyup)
import DOM.HTML.HTMLTextAreaElement (value)
import DOM.HTML.Types ( HTMLAnchorElement, HTMLTextAreaElement , HTMLUListElement , htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector , QuerySelector(QuerySelector))
import DOM.Node.Types (elementToEventTarget, ParentNode)
import Data.Array (nub, sort)
import Data.Maybe (Maybe, maybe)
import Prelude (Unit, unit, discard, bind, (<<<), (>>=), ($), (<$>), (<*>), (=<<))
import Util.DOM (updatePalette, updateDownloadLink)
import Util.Parse (parse)
import Util.Types (Color', Colors'(..))

--------------------------------------------------------------------------------

data Elements = Elements HTMLTextAreaElement HTMLUListElement HTMLAnchorElement

--------------------------------------------------------------------------------

extractColors :: String -> Array Color'
extractColors = (sort <<< nub <<< fromColors <<< parse) where
  fromColors (Colors' c) = c

--------------------------------------------------------------------------------

updateDOM :: Elements
          -> Array Color'
          -> forall eff. Eff (dom :: DOM, console :: CONSOLE | eff) Unit
updateDOM (Elements _ ul a) colors = do
  updatePalette      ul colors
  updateDownloadLink a  colors
  pure unit

--------------------------------------------------------------------------------

onTextChange :: forall eff. Elements
                         -> Event
                         -> Eff (dom :: DOM, console :: CONSOLE | eff) Unit
onTextChange elements@(Elements textarea ul a) e = extractColors <$> value textarea >>= updateDOM elements

--------------------------------------------------------------------------------

queryElements :: forall eff. ParentNode -> Eff (dom :: DOM | eff) (Maybe Elements)
queryElements docNode = do

  -- querySelector qs docNode :: Eff ( dom ∷ DOM | eff ) (Maybe Element)
  -- (=<<) fromElement        :: (Element -> Maybe SpecialisedElement)
  --                             -> Maybe Element
  --                             -> Maybe SpecialisedElement
  --
  -- Thus we lift that into Eff so that we can extract a Maybe SpecialisedElement

  textarea <- (=<<) fromElement <$> querySelector (QuerySelector "#in") docNode
  ul       <- (=<<) fromElement <$> querySelector (QuerySelector "#out") docNode
  a        <- (=<<) fromElement <$> querySelector (QuerySelector "#download") docNode

  pure $ Elements <$> textarea <*> ul <*> a

--------------------------------------------------------------------------------

bindDOM :: forall eff. Elements -> Eff (dom :: DOM, console :: CONSOLE | eff) Unit
bindDOM elements@(Elements textarea ul a) = addEventListener keyup listener true eventTarget
  where
    listener    = eventListener (onTextChange elements)
    eventTarget = (elementToEventTarget <<< toElement) textarea

--------------------------------------------------------------------------------

main :: forall eff. Eff (dom :: DOM, console :: CONSOLE | eff) Unit
main = window
  >>= document
  >>= (queryElements <<< htmlDocumentToParentNode)
  >>= (maybe (log "Could not access elements") bindDOM)

--------------------------------------------------------------------------------
