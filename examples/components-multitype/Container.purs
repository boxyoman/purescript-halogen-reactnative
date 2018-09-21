module Example.Container.MultiType.Container where

import Prelude

import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..))

import Example.Container.MultiType.ComponentA as CA
import Example.Container.MultiType.ComponentB as CB
import Example.Container.MultiType.ComponentC as CC
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.ReactNative as HR
import Halogen.ReactNative.Elements as HRE


data Query a = ReadStates a

type State =
  { a :: Maybe Boolean
  , b :: Maybe Int
  , c :: Maybe String
  }


type ChildQuery = Coproduct3 CA.Query CB.Query CC.Query

type ChildSlot = Either3 Unit Unit Unit


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
    { a : Nothing
    , b : Nothing
    , c : Nothing
    }

  render :: State -> HR.ParentVIEW Query ChildQuery ChildSlot m
  render state =
    HRE.view []
      [ HRE.text ""
      , HRE.view []
        [ HRE.text "Compnent A"
        , HR.slot' CP.cp1 unit CA.component unit absurd
        ]
      , HRE.text ""
      , HRE.view []
        [ HRE.text "Compnent B"
        , HR.slot' CP.cp2 unit CB.component unit absurd
        ]
      , HRE.text ""
      , HRE.view []
        [ HRE.text "Compnent C"
        , HR.slot' CP.cp3 unit CC.component unit absurd
        ]
      ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval = case _ of
    ReadStates next -> do
      a <- H.query' CP.cp1 unit (H.request CA.GetState)
      b <- H.query' CP.cp2 unit (H.request CB.GetCount)
      c <- H.query' CP.cp3 unit (H.request CC.GetValue)
      H.put { a, b, c }
      pure next
