module Example.Ajax.Component where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.ReactNative as HR
import Halogen.ReactNative.Elements as HRE
import Halogen.ReactNative.Events as HE
import Halogen.ReactNative.Properties as HP

import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response as AXResponse
import Effect.Aff (Aff)


type State =
  { loading :: Boolean
  , username :: String
  , result :: Maybe String
  }

data Query a
  = SetUsername String a
  | MakeRequest a


component :: H.Component HR.VIEW Query Unit Void Aff
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { loading: false, username: "", result: Nothing }

  render :: State -> HR.ComponentVIEW Query
  render state =
    HRE.view []
      [ HRE.text "" --Lazy way of dropping the text down
      , HRE.text "Lookup GitHub user"
      , HRE.text ""
      , HRE.text "Enter username:"
      , HRE.textInput
        [ HP.value state.username
        , HE.onChangeText (HE.input SetUsername)
        ] []
      , HRE.button
        [ HP.title "Fetch Info"
        , HP.disabled state.loading
        , HE.onPress (HE.input_ MakeRequest)
        ]
      , HRE.view []
        ( case state.result of
            Nothing -> []
            Just res ->
              [ HRE.text "response:"
              , HRE.text res
              ]
        )
      ]

  eval :: Query ~> H.ComponentDSL State Query Void Aff
  eval = case _ of
    SetUsername username next -> do
          H.modify_ (_ { username = username, result = Nothing :: Maybe String })
          pure next
    MakeRequest next -> do
      username <- H.gets _.username
      H.modify_ (_ { loading = true })
      response <- H.liftAff $ AX.get AXResponse.string ("https://api.github.com/users/" <> username)
      H.modify_ (_ { loading = false, result = Just response.response })
      pure next
