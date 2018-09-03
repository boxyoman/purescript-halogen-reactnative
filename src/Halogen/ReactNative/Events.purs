module Halogen.ReactNative.Events
  ( handler
  , touchHandler
  , onPress
  , module Halogen.HTML.Events
  ) where


import Prelude

import Halogen.HTML.Events (input, input_)
import Data.Maybe (Maybe)
import Halogen.Query.InputF (InputF(..))
import Halogen.Query (Action, action)
import Halogen.ReactNative.Properties (IProp(..))
import ReactNative.Basic (Prop(Handler), EventType(..), NativeEvent)
import ReactNative.EventTypes (TouchEvent)
import Unsafe.Coerce (unsafeCoerce)


handler :: forall r i. EventType -> (NativeEvent -> Maybe i) -> IProp r i
handler evt = IProp <<< Handler evt <<< map (map Query)


touchHandler :: forall i. (TouchEvent -> Maybe i) -> (NativeEvent -> Maybe i)
touchHandler = unsafeCoerce


onPress :: forall r i . (TouchEvent -> Maybe i) -> IProp (onPress :: TouchEvent | r) i
onPress = handler (EventType "onPress") <<< touchHandler
