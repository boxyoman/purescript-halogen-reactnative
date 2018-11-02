module Example.HOC.HOC where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Coyoneda (Coyoneda, unCoyoneda, liftCoyoneda)

import Halogen.Query.HalogenM as HQ
import Halogen as H
import Halogen.ReactNative as HR
import Halogen.ReactNative.Events as HE
import Halogen.ReactNative.Properties as HP
import Halogen.ReactNative.Elements as HRE


class CanSet f where
  set :: Boolean -> H.Action f


data Query f i o a
  -- | Example query for the HOC
  = ToggleOn a
  | Set a

  -- | Contains a query of the inner component
  | Inner (Coyoneda f a)
  -- | Handle messages of te inner component
  | HandleInner o a
  -- | React to input to the HOC
  | InnerInput i a


-- | Lift a query from the inner component to a query of the HOC. Useful when
-- | querying a component thats "inside" this HOC.
liftQuery :: forall f i o a. f a -> Query f i o a
liftQuery = Inner <<< liftCoyoneda


type Slot = Unit

type State i =
  -- | State of the HOC itself
  { on :: Boolean
  -- | Keep track of inputs that we pass through to the inner component
  , input :: i
  }


factory
  :: forall f i o m
   . CanSet f
  => H.Component HR.VIEW f i o m
  -> H.Component HR.VIEW (Query f i o) i o m
factory innerComponent =
  H.parentComponent
    { initialState: { on: true, input: _ }
    , render
    , eval
    , receiver: \i -> Just $ InnerInput i unit
    }

  where

  render :: State i -> HR.ParentVIEW (Query f i o) f Slot m
  render state =
    HRE.view []
      [ HRE.view []
        [ HRE.button
          [ HE.onPress (HE.input_ ToggleOn)
          , HP.title "Check now"
          ]
        , HRE.text $ " Wrapper state: " <> if state.on then "on" else "off"
        ]
      , HRE.view []
        [ HRE.button
          [ HE.onPress (HE.input_ Set)
          , HP.title "Set inner component to off"
          ]
        ]
      , HRE.view []
        [ HR.slot unit innerComponent state.input (HE.input HandleInner)
        ]
      ]

  eval :: Query f i o ~> H.ParentDSL (State i) (Query f i o) f Slot o m
  eval (ToggleOn next) = do
    H.modify_ $ \state -> state { on = not state.on }
    pure next
  eval (Set next) = do
    _ <- H.query unit $ H.action (set false)
    pure next
  eval (Inner iq) = iq # unCoyoneda \k q -> do
    result <- H.query unit q
    case result of
      Nothing ->
        HQ.halt "HOC inner component query failed (this should be impossible)"
      Just a -> pure (k a)
  eval (HandleInner o next) = do
    H.raise o
    pure next
  eval (InnerInput i next) = do
    H.modify_ $ _{ input = i }
    pure next
