module Halogen.ReactNative.Properties where


import Prelude

import Data.Newtype (class Newtype)
import Halogen.Query.InputF (InputF)
import ReactNative.Basic (Prop(..), propFromString, PropValue)


newtype IProp (r :: # Type) i = IProp (Prop (InputF Unit i))

derive instance functorIProp :: Functor (IProp r)
derive instance newtypeIProp :: Newtype (IProp r i) _

prop :: forall i r . String -> PropValue -> IProp r i
prop str = IProp <<< Property str

title :: forall i r . String -> IProp ( title :: String | r) i
title str = prop "title" $ propFromString str
