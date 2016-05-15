module Main (..) where

import Html
import Html.Events as Events
import Html.Attributes as Attr
import List
import String

import OneTask exposing (..)
-- MODEL

type alias Model =
  {
    currentInput : String,
    tasks : List (ID, OneTask.Model),
    nextId : ID
  }

type alias ID = Int

initialModel : Model
initialModel =
  {
    currentInput = "",
    tasks = [],
    nextId = 0
  }

oneTaskInitialModel : OneTask.Model
oneTaskInitialModel =
  OneTask.initialModel

type Action
  = InputChange String
  | GetTasks Model
  | NewTask
  | ModifyTask ID OneTask.Action
  | NoOp

-- UPDATE
update : Action -> Model -> Model
update action model =
  case action of
    InputChange input ->
      {model | currentInput = input}

    NewTask ->
      if (String.length model.currentInput) > 0
        then {
              model |
                tasks = model.tasks ++ [(model.nextId, { oneTaskInitialModel | text = model.currentInput })]
                , currentInput = ""
                , nextId = model.nextId + 1
            }
        else model

    ModifyTask id taskAction ->
      let
        updateTask (taskId, taskModel) =
          if taskId == id
            then (taskId, OneTask.update taskAction taskModel)
            else (taskId, taskModel)
        newTasks =
          List.map updateTask model.tasks
      in
        { model | tasks = List.filter (\(_, task) -> (String.length task.text) > 0) newTasks }

    GetTasks savedModel ->
      savedModel

    NoOp ->
      model

tasksMail : Signal.Mailbox Action
tasksMail =
  Signal.mailbox NoOp

modelSignal : Signal Model
modelSignal =
  Signal.foldp update initialModel (Signal.merge tasksMail.signal getTasks)

--VIEW

view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    tasks = List.map (taskView address) model.tasks
    newTaskInput = Html.input
                    [ Events.on "change" Events.targetValue (\input -> Signal.message address (InputChange input))
                    , Attr.class "new-task__input"
                    , Attr.value model.currentInput
                    ] []
    newTaskButton = Html.button [ Events.onClick address NewTask
                                , Attr.class "new-task__button"  ] [ Html.text "add task" ]
    newTaskControl = Html.div [ Attr.class "new-task" ] [newTaskInput, newTaskButton]
  in
    Html.div []
      [
        Html.div
        [ Attr.class "task__list"
        -- , Attr.style [("height", (toString ((List.length model.tasks) * 10)) ++ "vh")]
        ] tasks
      , newTaskControl
      ]

taskView : Signal.Address Action -> (ID, OneTask.Model) -> Html.Html
taskView address (id, taskModel) =
  OneTask.view (Signal.forwardTo address (ModifyTask id)) taskModel

main : Signal Html.Html
main =
  Signal.map (view tasksMail.address) modelSignal

getTasks : Signal Action
getTasks =
  Signal.map (\model -> GetTasks model) fromJS

port fromJS : Signal Model

port toJS : Signal Model
port toJS =
  modelSignal
