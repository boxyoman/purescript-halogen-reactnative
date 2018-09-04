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
import ReactNative.EventTypes (PressEvent, ScrollEvent, TextInputEvent, TouchEvent, SelectionChangeEvent, KeyPressEvent)
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

onResponderGrant :: forall r i . (TouchEvent -> Maybe i) -> IProp (onResponderGrant :: TouchEvent | r) i
onResponderGrant = handler (EventType "onResponderGrant") <<< touchHandler

onResponderMove :: forall r i . (TouchEvent -> Maybe i) -> IProp (onResponderMove :: TouchEvent | r) i
onResponderMove = handler (EventType "onResponderMove") <<< touchHandler

onResponderReject :: forall r i . (TouchEvent -> Maybe i) -> IProp (onResponderReject :: TouchEvent | r) i
onResponderReject = handler (EventType "onResponderReject") <<< touchHandler

onResponderRelease :: forall r i . (TouchEvent -> Maybe i) -> IProp (onResponderRelease :: TouchEvent | r) i
onResponderRelease = handler (EventType "onResponderRelease") <<< touchHandler

onResponderTerminate :: forall r i . (TouchEvent -> Maybe i) -> IProp (onResponderTerminate :: TouchEvent | r) i
onResponderTerminate = handler (EventType "onResponderTerminate") <<< touchHandler

onResponderTerminationRequest
  :: forall r i
   . (TouchEvent -> Maybe i)
  -> IProp (onResponderTerminationRequest :: TouchEvent | r) i
onResponderTerminationRequest =
  handler (EventType "onResponderTerminationRequest") <<< touchHandler

onChangeText :: forall r i . (TextInputEvent -> Maybe i) -> IProp (onChangeText :: TextInputEvent | r) i
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

