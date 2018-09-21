module Example.Container.MultiType.ComponentA where

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
  | GetState (Boolean -> a)


component :: forall m. H.Component HR.VIEW Query Unit Void m
component =
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

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Toggle next -> do
      H.modify_ not
      pure next
    GetState reply -> do
      state <- H.get
      pure (reply state)

