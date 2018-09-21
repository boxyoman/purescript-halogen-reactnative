module Example.Container.MultiType.ComponentC where

import Prelude
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.ReactNative as HR
import Halogen.ReactNative.Events as HE
import Halogen.ReactNative.Properties as HP
import Halogen.ReactNative.Elements as HRE


type State = String

data Query a
  = HandleInput String a
  | GetValue (String -> a)


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
  initialState = "Hello"

  render :: State -> HR.ComponentVIEW Query
  render state =
    HRE.view []
      [ HRE.text "What do you have to say?"
      , HRE.textInput
        [ HP.value state
        , HE.onChangeText (HE.input HandleInput)
        ] []
      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    HandleInput value next -> do
      H.put value
      pure next
    GetValue reply -> do
      state <- H.get
      pure (reply state)

