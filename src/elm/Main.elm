import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, keyCode, onInput, onClick)
import Json.Decode as Json
import Debug exposing (..)
import Material
import Material.Scheme
import Material.Button as Button
import Material.List as List
import Material.Options as Options exposing (css)

main = Html.beginnerProgram { model = model, view = view, update = update}

type alias TodoRecord = 
  { id : Int
  , text: String
  , completed: Bool
  }

type alias TodoList = 
  List TodoRecord

type alias Model =
  { current : String
  , todos : TodoList
  , completed: TodoList
  }

type Msg
  = TryAddTodo Int
  | UpdateText String
  | MarkAsCompleted Int
  | MarkAsIncomplete Int

model : Model
model =
  Model "" [] []


update : Msg -> Model -> Model
update msg model = 

  case msg of
    UpdateText text ->
      { model | current = text}
    TryAddTodo key -> 
      let _ = Debug.log ((toString key) ++ "TEST")
      in
      if key == 13 then
        let todo = TodoRecord (List.length model.todos) model.current False
        in
        updateTodos (resetCurrentText model) (model.todos ++ [todo])
      else 
        model
    MarkAsCompleted id ->
      let (todos, completed) = move model.todos model.completed id
      in
      updateTodos (updateCompleted model completed) todos
    MarkAsIncomplete id ->
      let (completed, todos) = move model.completed model.todos id
      in
      updateTodos (updateCompleted model completed) todos


view : Model -> Html Msg
view model = 
  div [id "main"]
    [
      div [id "input-container"]
        [ input [placeholder "Lets add a todo!", onInput UpdateText, onKeyDown TryAddTodo, value model.current] []
        ]
    , div [id "todo-list-container"]
        [
          renderTodos model,
          renderCompleteds model
        ]
    ]

move: TodoList -> TodoList -> Int -> (TodoList, TodoList)
move listA listB todoId = 
  (List.filter (idNotEquals todoId) listA, listB ++ (List.filter (idEquals todoId) listA))

addTodo: TodoRecord -> TodoList -> TodoList
addTodo record todos = 
  todos ++ [record]

renderTodos: Model -> Html Msg
renderTodos model = 
  renderRecords "Todos" renderTodo model.todos

renderCompleteds: Model -> Html Msg
renderCompleteds model = 
  renderRecords "Completed" renderCompleted model.completed

renderRecords: String -> (TodoRecord -> Html Msg) -> TodoList -> Html Msg
renderRecords recordType todoRenderer list = 
  div [hidden (List.isEmpty list)]
    [
      h3 [id "todo-header"]
        [text (recordType ++ ":")]
      , ul [id "todo-list"]
        (List.map todoRenderer list)
    ]

renderRecord: TodoRecord -> (Int -> Msg) -> Html Msg
renderRecord record tagger = 
  li[id (toString record.id), onClick (tagger record.id)]
    [text record.text]

renderTodo: TodoRecord -> Html Msg
renderTodo record = 
  renderRecord record MarkAsCompleted

renderCompleted: TodoRecord -> Html Msg
renderCompleted record = 
  renderRecord record MarkAsIncomplete

isIncomplete: TodoRecord -> Bool
isIncomplete record = 
  record.completed == False

isComplete: TodoRecord -> Bool
isComplete record = 
  record.completed == True

idEquals: Int -> TodoRecord -> Bool
idEquals id rec = 
  rec.id == id

idNotEquals: Int -> TodoRecord -> Bool
idNotEquals id rec = 
  rec.id /= id

onKeyDown: (Int -> Msg) -> Attribute Msg
onKeyDown tagger = 
  on "keydown" (Json.map tagger keyCode)

updateTodos: Model -> TodoList -> Model
updateTodos model todos =
  {model | todos = todos}

updateCompleted: Model -> TodoList -> Model
updateCompleted model completed =
  {model | completed = completed}

resetCurrentText: Model -> Model
resetCurrentText model = 
  {model | current = ""}