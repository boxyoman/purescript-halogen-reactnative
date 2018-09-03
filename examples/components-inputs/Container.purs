module Example.Components.Inputs.Container where

import Prelude
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.ReactNative as HR
import Halogen.ReactNative.Events as HE
import Halogen.ReactNative.Properties as HP
import Halogen.ReactNative.Elements as HRE
import Example.Components.Inputs.Display as Display


type State = Int

data Query a
  = Increment a
  | Decrement a


newtype Slot = Slot Int

derive newtype instance eqSlot :: Eq Slot
derive newtype instance ordSlot :: Ord Slot


component :: forall m. H.Component HR.VIEW Query Unit Void m
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = 1

  render :: State -> HR.ParentVIEW Query Display.Query Slot m
  render state =
    HRE.view []
      [ HRE.view []
        [ HR.slot (Slot 1) Display.component state absurd
        , HR.slot (Slot 2) Display.component (state*2) absurd
        , HR.slot (Slot 3) Display.component (state*3) absurd
        , HR.slot (Slot 4) Display.component (state*10) absurd
        , HR.slot (Slot 5) Display.component (state*state) absurd
        ]
      , HRE.button
        [ HE.onPress (HE.input_ Increment)
        , HP.title "+1"
        ]
      , HRE.button
        [ HE.onPress (HE.input_ Decrement)
        , HP.title "-1"
        ]
      ]

  eval :: Query ~> H.ParentDSL State Query Display.Query Slot Void m
  eval = case _ of
    Increment next -> do
      H.modify_ (_ + 1)
      pure next
    Decrement next -> do
      H.modify_ (_ - 1)
      pure next
