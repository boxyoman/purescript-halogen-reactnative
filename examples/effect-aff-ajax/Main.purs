module Example.Ajax.Main where

import Prelude
import Effect (Effect)

import Halogen.ReactNative.Driver (runUI)
import Halogen.Aff as HA

import Example.Ajax.Component as Container

main :: Effect Unit
main = HA.runHalogenAff do
  runUI Container.component unit "examples"

