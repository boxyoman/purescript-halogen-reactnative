module Halogen.ReactNative
  ( slot
  , module Halogen.ReactNative.Core
  , module Halogen.ReactNative.Elements
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen.Component (Component, mkComponentSlot, unComponent)
import Halogen.ReactNative.Core (ParentVIEW, VIEW, ComponentVIEW)
import Halogen.ReactNative.Elements (ButtonProps, Leaf, Node, button, text, text', view)
import Halogen.ReactNative.Core as Core

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
