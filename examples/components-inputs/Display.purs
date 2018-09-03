module Example.Components.Inputs.Display where

import Prelude

import Halogen as H
import Halogen.ReactNative.Core as HR
import Halogen.ReactNative.Events as HE
import Halogen.ReactNative.Elements as HRE

type Input = Int

type State = Int

data Query a = HandleInput Int a


component :: forall m. H.Component HR.VIEW Query Input Void m
component =
  H.component
    { initialState: identity
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  render :: State -> HR.ComponentVIEW Query
  render state =
    HRE.view []
      [ HRE.text "My input value is:"
      , HRE.text (show state)
      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    HandleInput n next -> do
      oldN <- H.get
      when (oldN /= n) $ H.put n
      pure next


