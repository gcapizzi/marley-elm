module Board where

import Column exposing (init, update, view)

import Html exposing (..)
import Html.Events exposing (onClick)
import Signal exposing (Address)

type alias ID = Int

type Action = AddColumn | Modify ID Column.Action

type alias Model =
  { columns : List (ID, Column.Model)
  , nextId : ID
  }

init : Model
init =
  { columns = [(1, Column.init)]
  , nextId = 2
  }

update : Action -> Model -> Model
update action model =
  case action of
    AddColumn ->
      { model |
        nextId = model.nextId + 1
      , columns = model.columns ++ [(model.nextId, Column.init)]
      }
    Modify id columnAction ->
      let updateColumn (columnId, columnModel) =
        if columnId == id
           then (columnId, Column.update columnAction columnModel)
           else (columnId, columnModel)
      in
         { model | columns = List.map updateColumn model.columns }

viewColumn : Address Action -> (ID, Column.Model) -> Html
viewColumn address (id, model) =
  Column.view (Signal.forwardTo address (Modify id)) model

view : Address Action -> Model -> Html
view address model =
  let columns = List.map (viewColumn address) model.columns
      addColumnButton = button [ onClick address AddColumn ] [ text "Add column" ]
  in
      div
      []
      (columns ++ [addColumnButton])
