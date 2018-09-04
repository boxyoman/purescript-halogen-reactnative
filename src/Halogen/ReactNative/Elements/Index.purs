module Halogen.ReactNative.Elements.Indexed where

import Prelude

import ReactNative.EventTypes (KeyPressEvent, PressEvent, ScrollEvent, TextInputEvent, TouchEvent, SelectionChangeEvent)

type ViewProps r =
  ( accessibilityLabel :: String
  , accessible :: Boolean
  , testID :: String
  , collapsable :: Boolean
  , accessibilityViewIsModal :: Boolean
  , accessibilityElementsHidden :: Boolean
  , shouldRasterizeIOS :: Boolean
  , onResponderMove :: TouchEvent
  | r
  )

type ButtonProps =
  ( onPress :: PressEvent
  , title :: String
  , accessibilityLabel :: String
  , disabled :: Boolean
  , testID :: String
  , hasTVPreferredFocus :: Boolean
  )

type TextInputProps = ViewProps
  ( onChangeText :: TextInputEvent
  , onKeyPress :: KeyPressEvent
  , onScroll :: ScrollEvent
  , onSubmitEditing :: Unit
  , onSelectionChange :: SelectionChangeEvent

  , allowFontScaling :: Boolean
  , autoCorrect :: Boolean
  , autoFocus :: Boolean
  , blurOnSubmit :: Boolean
  , caretHidden :: Boolean
  , clearTextOnFocus :: Boolean
  , contextMenuHidden :: Boolean
  , defaultValue :: String
  , disableFullscreenUI :: Boolean
  , editable :: Boolean
  , enablesReturnKeyAutomatically :: Boolean
  , inlineImageLeft :: String
  , inlineImagePadding  :: Number
  , maxLength :: Int
  , multiline :: Boolean
  , numberOfLines :: Int
  , secureTextEntry :: Boolean
  , placeholder :: String
  , returnKeyLabel :: String
  , selection :: { start :: Int, end :: Int }
  , selectTextOnFocus :: Boolean
  , spellCheck :: Boolean
  , value :: String
  )

type ActivityIndicatorProps = ViewProps
  ( animation :: Boolean
  , hidesWhenStopped :: Boolean
  )
