module ReactNative.Basic where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)

foreign import data NativeEvent :: Type
foreign import data PropValue :: Type


newtype EventType = EventType String

data Prop a
  = Handler EventType (NativeEvent -> Maybe a)
  | Nested String (Prop a)
  | Property String PropValue


propFromString :: String -> PropValue
propFromString = unsafeCoerce


element :: NativeClass -> NativeProps -> NativeElement
element = runFn2 element_

instance functorProp :: Functor Prop where
  map f (Handler evt cb) = Handler evt (map f <$> cb)
  map f (Nested s p) = Nested s (map f p)
  map _ p = unsafeCoerce p


-- Meant to a react class
foreign import data NativeClass :: Type

-- | React element
foreign import data NativeElement :: Type

-- | React Property
foreign import data NativeProps :: Type


instance semigroupNativeProps :: Semigroup NativeProps where
  append = runFn2 concatProps

instance monoidNativeProps :: Monoid NativeProps where
  mempty = emptyProps

foreign import concatProps :: Fn2 NativeProps NativeProps NativeProps

foreign import element_ :: Fn2 NativeClass NativeProps NativeElement

foreign import handlerProp :: Fn2 EventType (NativeEvent -> Effect Unit) NativeProps

foreign import prop :: forall value .  Fn2 String value NativeProps

foreign import emptyProps :: NativeProps

foreign import registerComponent :: String -> NativeElement -> Effect (NativeElement -> Effect Unit)
