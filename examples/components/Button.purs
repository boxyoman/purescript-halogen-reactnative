module Example.Container.Button where

import Prelude
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.ReactNative as HR
import Halogen.ReactNative.Events as HE
import Halogen.ReactNative.Properties as HP
import Halogen.ReactNative.Elements as HRE


type State = Boolean

data Query a
  = Toggle a
  | IsOn (Boolean -> a)

data Message = Toggled Boolean

myButton :: forall m. H.Component HR.VIEW Query Unit Message m
myButton =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = false

  render :: State -> HR.ComponentVIEW Query
  render state =
    let
      label = if state then "On" else "Off"
    in
      HRE.view []
        [ HRE.button
          [ HP.title label
          , HE.onPress (HE.input_ Toggle)
          ]
        , HRE.text label
        ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Toggle next -> do
      state <- H.get
      let nextState = not state
      H.put nextState
      H.raise $ Toggled nextState
      pure next
    IsOn reply -> do
      state <- H.get
      pure (reply state)

