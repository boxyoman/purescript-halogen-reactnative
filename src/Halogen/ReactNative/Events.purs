module Halogen.ReactNative.Events
  ( handler
  , touchHandler
  , pressHandler
  , changeTextHandler
  , keyPressHandler
  , scrollHandler
  , selectionChangeHandler
  , onPress
  , onResponderMove
  , onChangeText
  , onSelectionChange
  , onSubmitEditing
  , onKeyPress
  , module Halogen.HTML.Events
  ) where


import Prelude

import Data.Maybe (Maybe)
import Halogen.HTML.Events (input, input_)
import Halogen.Query.InputF (InputF(..))
import Halogen.ReactNative.Properties (IProp(..))
import ReactNative.Basic (EventType(..), NativeEvent, Prop(Handler))
import ReactNative.EventTypes
  ( PressEvent, ScrollEvent, TextInputEvent, TouchEvent, SelectionChangeEvent
  , KeyPressEvent
  )
import Unsafe.Coerce (unsafeCoerce)


handler :: forall r i. EventType -> (NativeEvent -> Maybe i) -> IProp r i
handler evt = IProp <<< Handler evt <<< map (map Query)


pressHandler :: forall i . (PressEvent -> Maybe i) -> (NativeEvent -> Maybe i)
pressHandler = unsafeCoerce

touchHandler :: forall i . (TouchEvent -> Maybe i) -> (NativeEvent -> Maybe i)
touchHandler = unsafeCoerce

changeTextHandler :: forall i. (String -> Maybe i) -> (NativeEvent -> Maybe i)
changeTextHandler = unsafeCoerce

keyPressHandler :: forall i . (KeyPressEvent -> Maybe i) -> (NativeEvent -> Maybe i)
keyPressHandler = unsafeCoerce

scrollHandler :: forall i . (ScrollEvent -> Maybe i) -> (NativeEvent -> Maybe i)
scrollHandler = unsafeCoerce

selectionChangeHandler :: forall i . (SelectionChangeEvent -> Maybe i) -> (NativeEvent -> Maybe i)
selectionChangeHandler = unsafeCoerce



onPress :: forall r i . (PressEvent -> Maybe i) -> IProp (onPress :: PressEvent | r) i
onPress = handler (EventType "onPress") <<< pressHandler

onResponderMove :: forall r i . (TouchEvent -> Maybe i) -> IProp (onChange :: TouchEvent | r) i
onResponderMove = handler (EventType "onResponderMove") <<< touchHandler

onChangeText :: forall r i . (TextInputEvent -> Maybe i) -> IProp (onChange :: TextInputEvent | r) i
onChangeText = handler (EventType "onChangeText") <<< changeTextHandler

onScroll :: forall r i . (ScrollEvent -> Maybe i) -> IProp (onScroll :: ScrollEvent | r) i
onScroll = handler (EventType "onScroll") <<< scrollHandler

onSelectionChange
  :: forall r i
   . (SelectionChangeEvent -> Maybe i)
  -> IProp (onSelectionChange :: SelectionChangeEvent | r) i
onSelectionChange = handler (EventType "onSelectionChange") <<< selectionChangeHandler

onSubmitEditing :: forall r i . (Unit -> Maybe i) -> IProp (onSubmitEditing :: Unit | r) i
onSubmitEditing = handler (EventType "onSubmitEditing") <<< pressHandler

onKeyPress :: forall r i . (KeyPressEvent -> Maybe i) -> IProp (onKeyPress :: KeyPressEvent | r) i
onKeyPress = handler (EventType "onKeyPress") <<< keyPressHandler

