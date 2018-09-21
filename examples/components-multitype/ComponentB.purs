module Example.Container.MultiType.ComponentB where

import Prelude
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.ReactNative as HR
import Halogen.ReactNative.Events as HE
import Halogen.ReactNative.Properties as HP
import Halogen.ReactNative.Elements as HRE


type State = Int

data Query a
  = Increment a
  | GetCount (Int -> a)


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
  initialState = 0

  render :: State -> HR.ComponentVIEW Query
  render state =
    HRE.view []
      [ HRE.text ("Current Value: " <> show state)
      , HRE.button
        [ HP.title "Increment"
        , HE.onPress (HE.input_ Increment)
        ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Increment next -> do
      H.modify_ (_ + 1)
      pure next
    GetCount reply -> do
      state <- H.get
      pure (reply state)

