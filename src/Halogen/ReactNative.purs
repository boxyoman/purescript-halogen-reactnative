module Halogen.ReactNative
  ( slot
  , slot'
  , module Halogen.ReactNative.Core
  , module Halogen.ReactNative.Elements
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.Component (Component, mkComponentSlot, unComponent)
import Halogen.Component.ChildPath (ChildPath, injSlot, prjQuery, injQuery)
import Halogen.ReactNative.Core (ParentVIEW, VIEW, ComponentVIEW)
import Halogen.ReactNative.Core as Core
import Halogen.ReactNative.Elements (button, scrollView, text, text', textInput, view)


slot
  :: forall f m p i o g
  . p
 -> Component VIEW g i o m
 -> i
 -> (o -> Maybe (f Unit))
 -> ParentVIEW f g p m
slot p component input outputQuery =
  let f = unComponent _.receiver component
  in Core.slot (mkComponentSlot p component input f outputQuery Just)


slot'
  :: forall f m p i o g g' p'
  . ChildPath g g' p p'
 -> p
 -> Component VIEW g i o m
 -> i
 -> (o -> Maybe (f Unit))
 -> ParentVIEW f g' p' m
slot' i p component input outputQuery =
  let pq = prjQuery i
      f = map (injQuery i) <<< unComponent _.receiver component
  in Core.slot (mkComponentSlot (injSlot i p) component input f outputQuery pq)
