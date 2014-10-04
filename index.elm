import Graphics.Input as Input
import Graphics.Input.Field as Field
import Graphics.Input.Field (Content, Selection, Direction(..))
import Window

-- Model

data Command =
  Start |
  Edit | EndEdit | Update Content

type Page = {
  name:String,
  content:String,
  editing:Maybe Content
  }

type State = Page

commands = Input.input Start

ignore = (Input.input ()).handle

-- Display

renderPage : Page -> Element
renderPage p = 
  case p.editing of
    Nothing ->
      asText p.name `above`
      plainText p.content |> Input.clickable commands.handle Edit
    Just c ->
      asText p.name `above`
      (Field.field Field.defaultStyle commands.handle (\u -> Update u) "" c |> Input.clickable ignore ())|> color red

renderScreen : (Int,Int) -> State -> Element
renderScreen (w,h) s =
  renderPage s 
  |> container w h middle |> Input.clickable commands.handle EndEdit

-- Update

step : Command -> State -> State
step c s = case c of
  Start -> s
  Edit -> { s | editing <- Just {string=s.content, selection=Selection 0 0 Forward} }
  Update c -> { s | editing <- Just c }
  EndEdit -> case s.editing of
    Just c -> { s | content <- c.string, editing <- Nothing }
    Nothing -> s

-- Signals

initial = {name="Welcome", content="fill me", editing=Nothing}
state = foldp step initial commands.signal

main = renderScreen <~ Window.dimensions ~ state