module Halogen.ReactNative.Properties where


import Prelude

import Data.Newtype (class Newtype)
import Halogen.Query.InputF (InputF)
import ReactNative.Basic (Prop(..), PropValue, toProp)


newtype IProp (r :: # Type) i = IProp (Prop (InputF Unit i))

derive instance functorIProp :: Functor (IProp r)
derive instance newtypeIProp :: Newtype (IProp r i) _

prop :: forall i r . String -> PropValue -> IProp r i
prop str = IProp <<< Property str

disabled :: forall i r . Boolean -> IProp ( disabled :: Boolean | r) i
disabled = prop "disabled" <<< toProp

accessible :: forall i r . Boolean -> IProp ( accessible :: Boolean | r) i
accessible = prop "accessible" <<< toProp

accessibilityLabel :: forall i r . String -> IProp ( accessibilityLabel :: String | r) i
accessibilityLabel = prop "accessibilityLabel" <<< toProp

accessibilityViewIsModal :: forall i r . Boolean -> IProp ( accessibilityViewIsModal :: Boolean | r) i
accessibilityViewIsModal = prop "accessibilityViewIsModal" <<< toProp

accessibilityElementsHidden :: forall i r . Boolean -> IProp ( accessibilityElementsHidden :: Boolean | r) i
accessibilityElementsHidden = prop "accessibilityElementsHidden" <<< toProp

allowFontScaling :: forall i r . Boolean -> IProp ( allowFontScaling :: Boolean | r) i
allowFontScaling = prop "allowFontScaling" <<< toProp

autoCorrect :: forall i r . Boolean -> IProp ( autoCorrect :: Boolean | r) i
autoCorrect = prop "autoCorrect" <<< toProp

autoFocus :: forall i r . Boolean -> IProp ( autoFocus :: Boolean | r) i
autoFocus = prop "autoFocus" <<< toProp

blurOnSubmit :: forall i r . Boolean -> IProp ( blurOnSubmit :: Boolean | r) i
blurOnSubmit = prop "blurOnSubmit" <<< toProp

caretHidden :: forall i r . Boolean -> IProp ( caretHidden :: Boolean | r) i
caretHidden = prop "caretHidden" <<< toProp

clearTextOnFocus :: forall i r . Boolean -> IProp ( clearTextOnFocus :: Boolean | r) i
clearTextOnFocus = prop "clearTextOnFocus" <<< toProp

collapsable :: forall i r . Boolean -> IProp ( collapsable :: Boolean | r) i
collapsable = prop "collapsable" <<< toProp

contextMenuHidden :: forall i r . Boolean -> IProp ( contextMenuHidden :: Boolean | r) i
contextMenuHidden = prop "contextMenuHidden" <<< toProp

defaultValue :: forall i r . String -> IProp ( defaultValue :: String | r) i
defaultValue = prop "defaultValue" <<< toProp

disableFullscreenUI :: forall i r . Boolean -> IProp ( disableFullscreenUI :: Boolean | r) i
disableFullscreenUI = prop "disableFullscreenUI" <<< toProp

editable :: forall i r . Boolean -> IProp ( editable :: Boolean | r) i
editable = prop "editable" <<< toProp

enablesReturnKeyAutomatically :: forall i r . Boolean -> IProp ( enablesReturnKeyAutomatically :: Boolean | r) i
enablesReturnKeyAutomatically = prop "enablesReturnKeyAutomatically" <<< toProp

hasTVPreferredFocus :: forall i r . Boolean -> IProp ( hasTVPreferredFocus :: Boolean | r) i
hasTVPreferredFocus = prop "hasTVPreferredFocus" <<< toProp

inlineImageLeft :: forall i r . String -> IProp ( inlineImageLeft :: String | r) i
inlineImageLeft = prop "inlineImageLeft" <<< toProp

inlineImagePadding :: forall i r . Number -> IProp ( inlineImagePadding :: Number | r) i
inlineImagePadding = prop "inlineImagePadding" <<< toProp

maxLength :: forall i r . Int -> IProp ( maxLength :: Int | r) i
maxLength = prop "maxLength" <<< toProp

multiline :: forall i r . Boolean -> IProp ( multiline :: Boolean | r) i
multiline = prop "multiline" <<< toProp

numberOfLines :: forall i r . Int -> IProp ( numberOfLines :: Int | r) i
numberOfLines = prop "numberOfLines" <<< toProp

secureTextEntry :: forall i r . Boolean -> IProp ( secureTextEntry :: Boolean | r) i
secureTextEntry = prop "secureTextEntry" <<< toProp

placeholder :: forall i r . String -> IProp ( placeholder :: String | r) i
placeholder = prop "placeholder" <<< toProp

returnKeyLabel :: forall i r . String -> IProp ( returnKeyLabel :: String | r) i
returnKeyLabel = prop "returnKeyLabel" <<< toProp

selection :: forall i r . { start :: Int, end :: Int } -> IProp ( selection :: { start :: Int, end :: Int } | r) i
selection = prop "selection" <<< toProp

selectTextOnFocus :: forall i r . Boolean -> IProp ( selectTextOnFocus :: Boolean | r) i
selectTextOnFocus = prop "selectTextOnFocus" <<< toProp

shouldRasterizeIOS :: forall i r . Boolean -> IProp ( shouldRasterizeIOS :: Boolean | r) i
shouldRasterizeIOS = prop "shouldRasterizeIOS" <<< toProp

testID :: forall i r . String -> IProp ( testID :: String | r) i
testID = prop "testID" <<< toProp

title :: forall i r . String -> IProp ( title :: String | r) i
title = prop "title" <<< toProp

value :: forall i r . String -> IProp ( value :: String | r) i
value = prop "value" <<< toProp

