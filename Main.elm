port module Main exposing (..)

import Html
import Html.Events as Events
import Html.Attributes as Attr
import Html.App
import List
import String

import Todo exposing (..)

main =
  Html.App.program {
    init = init,
    view = view,
    update = update,
    subscriptions = subscriptions
  }

-- MODEL

type alias Model =
  {
    currentInput : String,
    todos : List (ID, Todo.Model),
    nextId : ID
  }

type alias ID = Int

init : (Model, Cmd Msg)
init =
  (Model "" [] 0, Cmd.none)

oneTodoInitialModel : Todo.Model
oneTodoInitialModel =
  Todo.init

type Msg
  = InputChange String
  | GetTodos Model
  | NewTodo
  | ModifyTodo ID Todo.Msg
  | NoOp

-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InputChange input ->
      ({model | currentInput = input}, Cmd.none)

    NewTodo ->
      if (String.length model.currentInput) > 0
        then
          let
            newModel =
              {
                model |
                  todos = model.todos ++ [(model.nextId, { oneTodoInitialModel | text = model.currentInput })]
                  , currentInput = ""
                  , nextId = model.nextId + 1
              }
          in
            (newModel, setTodosInJS newModel)
        else (model, setTodosInJS model)

    ModifyTodo id todoMsg ->
      let
        updateTodo (todoId, todoModel) =
          if todoId == id
            then (todoId, Todo.update todoMsg todoModel)
            else (todoId, todoModel)
        newTodos =
          List.map updateTodo model.todos
        newModel =
          { model | todos = List.filter (\(_, todo) -> (String.length todo.text) > 0) newTodos }
      in
        (newModel, setTodosInJS newModel)

    GetTodos savedModel ->
      (savedModel, Cmd.none)

    NoOp ->
      (model, Cmd.none)

--VIEW

view : Model -> Html.Html Msg
view model =
  let
    todos = List.map todoView model.todos
    newTodoInput = Html.input
                    [ Events.onInput InputChange
                    , Attr.class "new-todo__input"
                    , Attr.value model.currentInput
                    ] []
    newTodoButton = Html.button [ Events.onClick NewTodo
                                , Attr.class "new-todo__button"  ] [ Html.text "add todo" ]
    newTodoControl = Html.div [ Attr.class "new-todo" ] [newTodoInput, newTodoButton]
  in
    Html.div []
      [
        Html.div
        [ Attr.class "todo__list"
        -- , Attr.style [("height", (toString ((List.length model.todos) * 10)) ++ "vh")]
        ]
        todos
      , newTodoControl
      ]

todoView : (ID, Todo.Model) -> Html.Html Msg
todoView (id, todoModel) =
  Html.App.map (ModifyTodo id) (Todo.view todoModel)

port getTodosFromJS : (Model -> msg) -> Sub msg
port setTodosInJS : Model -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions model =
  getTodosFromJS GetTodos
