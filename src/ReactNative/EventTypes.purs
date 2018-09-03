module ReactNative.EventTypes
  ( TouchEvent
  , LayoutEvent
  , TextInputEvent
  , ContentSizeEvent
  , ScrollEvent
  ) where

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

type TextInputEvent = {
  nativeEvent :: {
    text :: String
  }
}

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
