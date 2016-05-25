module Todo exposing (..)

import Html
import Html.Attributes as Attr
import Html.Events as Events
import List
import String
import Array exposing (get, fromList)

-- MODEL
type alias STID = Int
type alias SubTodoText = String
type alias IsChecked = Bool

type alias SubTodo =
  {
    id : STID
  , text: SubTodoText
  , checked: IsChecked
  }

type alias Model =
  {
    text : String,
    subTodos : List SubTodo,
    showSubTodos : Bool,
    currentSubTodoInput : String,
    lastSTID: STID
  }

initialSubTodos : List SubTodo
initialSubTodos =
  [ { id = 0
    , text = "Subtodo num. 1"
    , checked = False
    }
  , { id = 1
    , text = "Subtodo num. 2"
    , checked = True
    }
  ]

init : Model
init =
  {
    text = "",
    subTodos = [],
    showSubTodos = False,
    currentSubTodoInput = "",
    lastSTID = 0
  }

-- UPDATE

type Msg
  = ToggleFolding
  | SubTodoInputChange String
  | AddSubTodo
  | ToggleSubTodo STID
  | DeleteTodo
  | NoOp

update : Msg -> Model -> Model
update msg model =
  case msg of
    ToggleFolding ->
      { model | showSubTodos = not model.showSubTodos}

    SubTodoInputChange text ->
      { model | currentSubTodoInput = text}

    AddSubTodo ->
      if (String.length model.currentSubTodoInput) > 0
        then  { model |
                subTodos =
                    model.subTodos ++
                      [{
                          id = model.lastSTID
                        , text = model.currentSubTodoInput
                        , checked = False
                      }],
                currentSubTodoInput = "",
                lastSTID = model.lastSTID + 1 }
        else model

    ToggleSubTodo stid ->
      let
        toggleChecking subT =
          if
            subT.id == stid
          then
            -- (id, text, (not checked))
            {subT | checked = not subT.checked}
          else
            subT

        newSubTodos =
          List.map
            toggleChecking
            model.subTodos
      in
        { model | subTodos = newSubTodos }

    DeleteTodo ->
      { model | text = "" }

    NoOp ->
      model

-- VIEW

deleteButton : Model -> Html.Html Msg
deleteButton model =
  Html.div
    [ Attr.class "dlt-btn"
    , Events.onClick DeleteTodo ]
    [
      Html.text "Delete"
    ]

view : Model -> Html.Html Msg
view model =
  let
    todoClass =
      if
        model.showSubTodos
      then
        "line todo__line todo__line_open"
      else
        "line todo__line"

    loaderHeight =
      toString
      <| (*) 100
      <| (\allChecked -> allChecked / (toFloat (List.length model.subTodos)))
      <| toFloat
      <| List.length
      <| List.filter .checked model.subTodos

    todoText =
      Html.div
        [Events.onClick ToggleFolding, Attr.class todoClass]
        [
          Html.text (model.text)
        , Html.div
          [ Attr.class "todo__loader"
          , Attr.style [("height", loaderHeight ++ "%")]
          , Attr.attribute "data-h" loaderHeight
          ] []
        , deleteButton model
        ]

    subTodosInputWButton =
      -- if model.showSubTodos
      --   then
          Html.div [ Attr.class "line subTodo__inWBtn"]
              [
                Html.input
                [
                  Events.onInput
                    (\input -> (SubTodoInputChange input))
                  , Attr.value model.currentSubTodoInput
                  , Attr.class "subtodo__input"
                ] [],
                Html.button [ Events.onClick AddSubTodo
                            , Attr.class "subtodo__button"  ] [ Html.text "add subtodo"]
              ]
        -- else Html.text ""

    subHeightsArray =
      createIndexPairs model.subTodos
      |> List.foldr (divideBy (\(i, subT) -> subT.checked)) []
      |> createHeightsList
      |> fromList

    subTodoLine i subT =
      let
        subTodoClass =
          if
            subT.checked
          then
            "line subtodo subtodo__checked"
          else
            "line subtodo"

        getIntToString m =
          case m of
            Just h ->
              toString h
            _ ->
              "0"
      in
        Html.div
          [
            Attr.class subTodoClass
          -- , Attr.attribute "data-h" (toString ( fromMaybe (get i (fromList subHeightsArray))))
          , Attr.style [("top", (getIntToString (get i subHeightsArray)) ++ "vw")]
          , Events.onClick (ToggleSubTodo subT.id)
          ]
          [ Html.text subT.text ]

    subTodos =
      Html.div
          [
            Attr.class "subtodo__list"
          , Attr.style
            [
              ("height",
                if model.showSubTodos
                  then (toString (((List.length model.subTodos) + 1) * 15)) ++ "vw"
                  else "0vw"
              )
            ]
          ]
          (
            List.concat [List.indexedMap subTodoLine model.subTodos, [subTodosInputWButton]]
          )

  in
    Html.div []
      [ todoText
      , subTodos
      -- , subTodosInputWButton
      ]

-- UTILS
createIndexPairs : List a -> List (Int, a)
createIndexPairs sbts =
  List.indexedMap (\i sbt -> (i, sbt)) sbts

divideBy : (a -> Bool) -> a -> List a -> List a
divideBy fn thing lst =
  if fn thing
    then lst ++ [thing]
    else thing :: lst

createHeightsList : List (Int, SubTodo) -> List Int
createHeightsList sbts =
  List.indexedMap (\i (ind, _) -> (ind, i * 15)) sbts
  |> List.sortBy (\(i, _) -> i)
  |> List.map (\(_, h) -> h)
