module Halogen.ReactNative.Unsafe.Elements where


import ReactNative.Basic (NativeClass, NativeElement)
import Unsafe.Coerce (unsafeCoerce)


foreign import buttonU :: NativeClass
foreign import textU :: NativeClass
foreign import viewU :: NativeClass
foreign import textInputU :: NativeClass
foreign import statusBarU :: NativeClass
foreign import scrollViewU :: NativeClass
foreign import activityIndicatorU :: NativeClass
foreign import touchableOpacityU :: NativeClass

textElemU :: String -> NativeElement
textElemU = unsafeCoerce
