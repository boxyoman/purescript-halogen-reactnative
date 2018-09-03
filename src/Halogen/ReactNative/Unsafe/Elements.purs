module Halogen.ReactNative.Unsafe.Elements where


import ReactNative.Basic (NativeClass, NativeElement)
import Unsafe.Coerce (unsafeCoerce)


foreign import buttonU :: NativeClass
foreign import textU :: NativeClass
foreign import viewU :: NativeClass

textElemU :: String -> NativeElement
textElemU = unsafeCoerce
