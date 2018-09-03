module Halogen.ReactNative.Elements
  ( Node
  , Leaf
  , ButtonProps
  , button
  , text'
  , text
  , view
  ) where

import Prelude

import Data.Newtype (unwrap)
import Halogen.ReactNative.Core (VIEW(..), Native(..))
import Halogen.ReactNative.Properties (IProp)
import Halogen.ReactNative.Unsafe.Elements (buttonU, textU, viewU)
import ReactNative.Basic (NativeClass)
import ReactNative.EventTypes (TouchEvent)
import Unsafe.Coerce (unsafeCoerce)

type Node r p i = Array (IProp r i) -> Array (VIEW p i) -> VIEW p i
type Leaf r p i = Array (IProp r i) -> VIEW p i

type ButtonProps = (
    onPress :: TouchEvent
  , title :: String
)

element :: forall r p i . NativeClass -> Array (IProp r i) -> Array (VIEW p i) -> VIEW p i
element c props children = VIEW $ Elem c (unsafeCoerce props) (unsafeCoerce children)

button :: forall p i . Leaf ButtonProps p i
button props =
  VIEW $ Elem buttonU (map unwrap props) []

text' :: forall p i . Node () p i
text' = element textU

text :: forall p i . String -> VIEW p i
text = VIEW <<< Text

view :: forall p i . Node () p i
view = element viewU
