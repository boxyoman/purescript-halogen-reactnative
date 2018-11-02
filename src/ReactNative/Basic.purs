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


class ToProp a where
  toProp :: a -> PropValue


instance stringToProp :: ToProp String where
  toProp = unsafeCoerce

instance intToProp :: ToProp Int where
  toProp = unsafeCoerce

instance booleanToProp :: ToProp Boolean where
  toProp = unsafeCoerce

instance numberToProp :: ToProp Number where
  toProp = unsafeCoerce

instance recordToProp :: ToProp (Record a) where
  toProp = unsafeCoerce

instance arrayToProp :: ToProp a => ToProp (Array a) where
  toProp = unsafeCoerce


element :: NativeClass -> NativeProps -> NativeElement
element = runFn2 element_


updateState :: NativeElement -> NativeThis -> Effect Unit
updateState = runFn2 updateState_


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

-- | Not really sure
foreign import data NativeThis :: Type


instance semigroupNativeProps :: Semigroup NativeProps where
  append = runFn2 concatProps

instance monoidNativeProps :: Monoid NativeProps where
  mempty = emptyProps

foreign import concatProps :: Fn2 NativeProps NativeProps NativeProps

foreign import element_ :: Fn2 NativeClass NativeProps NativeElement

foreign import handlerProp :: Fn2 EventType (NativeEvent -> Effect Unit) NativeProps

foreign import prop :: forall value .  Fn2 String value NativeProps

foreign import emptyProps :: NativeProps

foreign import updateState_ :: Fn2 NativeElement NativeThis (Effect Unit)

foreign import mkComponent :: NativeElement -> { rclass :: NativeClass, self :: NativeThis }

foreign import registerComponent :: String -> NativeClass -> Effect Unit
