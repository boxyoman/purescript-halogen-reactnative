module Example.HOC.Container where

import Prelude
import Data.Maybe (Maybe(..), maybe)

import Halogen as H
import Halogen.ReactNative as HR
import Halogen.ReactNative.Events as HE
import Halogen.ReactNative.Properties as HP
import Halogen.ReactNative.Elements as HRE
import Example.HOC.Button as Button
import Example.HOC.HOC as HOC

data Query a
  = HandleButton Button.Message a
  | CheckButtonState a

type State =
  { toggleCount :: Int
  , buttonState :: Maybe Boolean
  }

data Slot = ButtonSlot
derive instance eqButtonSlot :: Eq Slot
derive instance ordButtonSlot :: Ord Slot

type ChildQuery = HOC.Query Button.Query Unit Button.Message

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
  initialState =
    { toggleCount: 0
    , buttonState: Nothing }

  render :: State -> HR.ParentVIEW Query ChildQuery Slot m
  render state =
    HRE.view []
      [ HR.slot ButtonSlot (HOC.factory Button.myButton) unit (HE.input HandleButton)
      , HRE.text ("Button has been toggled " <> show state.toggleCount <> " time(s)")
      , HRE.text
        $ "Last time I checked, the button was: "
        <> (maybe "(not checked yet)" (if _ then "on" else "off") state.buttonState)
        <> ". "
      , HRE.button
        [ HP.title "Check now"
        , HE.onPress (HE.input_ CheckButtonState)
        ]
      ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery Slot Void m
  eval = case _ of
    HandleButton (Button.Toggled _) next -> do
      H.modify_ (\st -> st { toggleCount = st.toggleCount + 1 })
      pure next
    CheckButtonState next -> do
      buttonState <- H.query ButtonSlot $ HOC.liftQuery $ H.request Button.IsOn
      H.modify_ (_ { buttonState = buttonState })
      pure next
