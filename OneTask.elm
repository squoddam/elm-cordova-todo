module OneTask exposing (..)

import Html
import Html.Attributes as Attr
import Html.Events as Events
import List
import String
import Array exposing (get, fromList)

-- MODEL
type alias STID = Int
type alias SubTaskText = String
type alias IsChecked = Bool

type alias SubTask =
  {
    id : STID
  , text: SubTaskText
  , checked: IsChecked
  }

type alias Model =
  {
    text : String,
    subTasks : List SubTask,
    showSubTasks : Bool,
    currentSubTaskInput : String,
    lastSTID: STID
  }

initialSubTasks : List SubTask
initialSubTasks =
  [ { id = 0
    , text = "Subtask num. 1"
    , checked = False
    }
  , { id = 1
    , text = "Subtask num. 2"
    , checked = True
    }
  ]

init : Model
init =
  {
    text = "",
    subTasks = [],
    showSubTasks = False,
    currentSubTaskInput = "",
    lastSTID = 0
  }

-- UPDATE

type Msg
  = ToggleFolding
  | SubTaskInputChange String
  | AddSubTask
  | ToggleSubTask STID
  | DeleteTask
  | NoOp

update : Msg -> Model -> Model
update msg model =
  case msg of
    ToggleFolding ->
      { model | showSubTasks = not model.showSubTasks}

    SubTaskInputChange text ->
      { model | currentSubTaskInput = text}

    AddSubTask ->
      if (String.length model.currentSubTaskInput) > 0
        then  { model |
                subTasks =
                    model.subTasks ++
                      [{
                          id = model.lastSTID
                        , text = model.currentSubTaskInput
                        , checked = False
                      }],
                currentSubTaskInput = "",
                lastSTID = model.lastSTID + 1 }
        else model

    ToggleSubTask stid ->
      let
        toggleChecking subT =
          if
            subT.id == stid
          then
            -- (id, text, (not checked))
            {subT | checked = not subT.checked}
          else
            subT

        newSubTasks =
          List.map
            toggleChecking
            model.subTasks
      in
        { model | subTasks = newSubTasks }

    DeleteTask ->
      { model | text = "" }

    NoOp ->
      model

-- VIEW

deleteButton : Model -> Html.Html Msg
deleteButton model =
  Html.div
    [ Attr.class "dlt-btn"
    , Events.onClick DeleteTask ]
    [
      Html.text "Delete"
    ]

view : Model -> Html.Html Msg
view model =
  let
    taskClass =
      if
        model.showSubTasks
      then
        "line task__line task__line_open"
      else
        "line task__line"

    loaderHeight =
      toString
      <| (*) 100
      <| (\allChecked -> allChecked / (toFloat (List.length model.subTasks)))
      <| toFloat
      <| List.length
      <| List.filter .checked model.subTasks

    taskText =
      Html.div
        [Events.onClick ToggleFolding, Attr.class taskClass]
        [
          Html.text (model.text)
        , Html.div
          [ Attr.class "task__loader"
          , Attr.style [("height", loaderHeight ++ "%")]
          , Attr.attribute "data-h" loaderHeight
          ] []
        , deleteButton model
        ]

    subTasksInputWButton =
      -- if model.showSubTasks
      --   then
          Html.div [ Attr.class "line subTask__inWBtn"]
              [
                Html.input
                [
                  Events.onInput
                    (\input -> (SubTaskInputChange input))
                  , Attr.value model.currentSubTaskInput
                  , Attr.class "subtask__input"
                ] [],
                Html.button [ Events.onClick AddSubTask
                            , Attr.class "subtask__button"  ] [ Html.text "add subtask"]
              ]
        -- else Html.text ""

    subHeightsArray =
      createIndexPairs model.subTasks
      |> List.foldr (divideBy (\(i, subT) -> subT.checked)) []
      |> createHeightsList
      |> fromList

    subTaskLine i subT =
      let
        subTaskClass =
          if
            subT.checked
          then
            "line subtask subtask__checked"
          else
            "line subtask"

        getIntToString m =
          case m of
            Just h ->
              toString h
            _ ->
              "0"
      in
        Html.div
          [
            Attr.class subTaskClass
          -- , Attr.attribute "data-h" (toString ( fromMaybe (get i (fromList subHeightsArray))))
          , Attr.style [("top", (getIntToString (get i subHeightsArray)) ++ "vw")]
          , Events.onClick (ToggleSubTask subT.id)
          ]
          [ Html.text subT.text ]

    subTasks =
      Html.div
          [
            Attr.class "subtask__list"
          , Attr.style
            [
              ("height",
                if model.showSubTasks
                  then (toString (((List.length model.subTasks) + 1) * 15)) ++ "vw"
                  else "0vw"
              )
            ]
          ]
          (
            List.concat [List.indexedMap subTaskLine model.subTasks, [subTasksInputWButton]]
          )

  in
    Html.div []
      [ taskText
      , subTasks
      -- , subTasksInputWButton
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

createHeightsList : List (Int, SubTask) -> List Int
createHeightsList sbts =
  List.indexedMap (\i (ind, _) -> (ind, i * 15)) sbts
  |> List.sortBy (\(i, _) -> i)
  |> List.map (\(_, h) -> h)
