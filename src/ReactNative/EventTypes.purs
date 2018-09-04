module ReactNative.EventTypes
  ( PressEvent
  , TouchEvent
  , LayoutEvent
  , TextInputEvent
  , ContentSizeEvent
  , ScrollEvent
  , SelectionChangeEvent
  , KeyPressEvent
  ) where

import Prelude

type PressEvent = Unit

type TouchEvent = {
  nativeEvent :: {
      identifier :: Int
    , locationX :: Number
    , locationY :: Number
    , pageX :: Number
    , pageY :: Number
    , timestamp :: Int
  }
}

type LayoutEvent = {
  nativeEvent :: {
    layout :: {
        x :: Int
      , y :: Int
      , width :: Int
      , height :: Int
    }
  }
}

type TextInputEvent = String

type ContentSizeEvent = {
  nativeEvent :: {
    contentSize :: { width :: Int, height :: Int }
  }
}

type ScrollEvent = {
  nativeEvent :: {
    contentOffset :: {x::Number, y::Number}
  }
}

type SelectionChangeEvent = {
  nativeEvent :: {
    selection :: {
      start :: Int,
      end  :: Int
    }
  }
}

type KeyPressEvent = {
  nativeEvent :: {
    key :: String
  }
}
