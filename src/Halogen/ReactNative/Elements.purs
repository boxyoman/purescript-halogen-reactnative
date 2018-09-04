module Halogen.ReactNative.Elements
  ( Node
  , Leaf
  , button
  , text'
  , text
  , view
  , textInput
  , statusBar
  , scrollView
  ) where

import Prelude

import Data.Newtype (unwrap)
import Halogen.ReactNative.Core (VIEW(..), Native(..))
import Halogen.ReactNative.Properties (IProp)
import Halogen.ReactNative.Unsafe.Elements as Elems
import ReactNative.Basic (NativeClass)
import Unsafe.Coerce (unsafeCoerce)
import Halogen.ReactNative.Elements.Indexed (ButtonProps, TextInputProps)

type Node r p i = Array (IProp r i) -> Array (VIEW p i) -> VIEW p i
type Leaf r p i = Array (IProp r i) -> VIEW p i


element :: forall r p i . NativeClass -> Array (IProp r i) -> Array (VIEW p i) -> VIEW p i
element c props children = VIEW $ Elem c (unsafeCoerce props) (unsafeCoerce children)

button :: forall p i . Leaf ButtonProps p i
button props =
  VIEW $ Elem Elems.buttonU (map unwrap props) []

text' :: forall p i . Node () p i
text' = element Elems.textU

text :: forall p i . String -> VIEW p i
text = VIEW <<< Text

view :: forall p i . Node () p i
view = element Elems.viewU

textInput :: forall p i . Node TextInputProps p i
textInput = element Elems.textInputU

statusBar :: forall p i . Node () p i
statusBar = element Elems.statusBarU

scrollView :: forall p i . Node () p i
scrollView = element Elems.scrollViewU

activityIndicator :: forall p i . Node () p i
activityIndicator = element Elems.activityIndicatorU
