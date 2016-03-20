module Column where

import String
import Html exposing (..)
import Html.Attributes exposing (class, value)
import Html.Events exposing (on, targetValue, keyCode)
import Json.Decode
import Signal exposing (Address)

type Action = AddCard | UpdateField String

type alias Card = { title : String }

type alias Model =
  { cards : List Card
  , field : String
  }

init : Model
init =
  { cards = []
  , field = ""
  }

update : Action -> Model -> Model
update action model =
  case action of
    UpdateField text ->
      { model |
        field = text }
    AddCard ->
      if (String.isEmpty model.field) then
        model
      else
        { model |
          field = "",
          cards = model.cards ++ [{ title = model.field }]
        }

card : Card -> Html
card c =
  article
    [ class "card" ]
    [ text c.title ]

is13 : Int -> Result String ()
is13 code = if code == 13 then Ok () else Err "not the right key code"

onEnter : Address a -> a -> Attribute
onEnter address value =
  on "keydown"
    (Json.Decode.customDecoder keyCode is13)
    (\_ -> Signal.message address value)

newCardInput address model =
  input
    [ value model.field
    , on "input" targetValue (Signal.message address << UpdateField)
    , onEnter address AddCard
    ]
    []

view : Address Action -> Model -> Html
view address model =
  section
  [ class "column" ]
  ((List.map card model.cards) ++ [newCardInput address model])
