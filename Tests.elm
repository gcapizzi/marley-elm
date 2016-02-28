module Tests where

import ElmTest exposing (..)

import Board
import Column

columnTests = [
    test "Updating the column text field" (
      let model = { cards = [], field = "" }
          newModel = Column.update (Column.UpdateField "foo") model
      in (assert (newModel == { cards = [], field = "foo" }))),
    test "Adding a card to a column" (
      let model = { cards = [{ title = "foo" }], field = "bar" }
          newModel = Column.update (Column.AddCard) model
      in (assert (newModel == {cards = [{ title = "foo" }, { title = "bar" }], field = "" }))),
    test "Adding a card with an empty title" (
      let model = { cards = [{ title = "foo" }], field = "" }
          newModel = Column.update (Column.AddCard) model
      in (assert (newModel == model)))
  ]

boardTests = [
  test "Adding a column" (
    let oldColumn = (1, { cards = [{ title = "foo" }], field = "" })
        model = { columns = [oldColumn], nextId = 2 }
        newModel = Board.update Board.AddColumn model
    in (assert (newModel == { columns = [oldColumn, (2, { cards = [], field = "" })], nextId = 3 }))),
  test "Modifying a column" (
    let firstColumn = (1, { cards = [], field = "" })
        lastColumn = (3, { cards = [], field = "bar" })
        model = { columns = [firstColumn, (2, { cards = [], field = "foo" }), lastColumn], nextId = 3 }
        newModel = Board.update (Board.Modify 2 Column.AddCard) model
    in (assert (newModel == { columns = [firstColumn, (2, { cards = [{ title = "foo" }], field = "" }), lastColumn], nextId = 3 })))
  ]

all : Test
all = suite "All tests" (columnTests ++ boardTests)
