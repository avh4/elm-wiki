import Graphics.Input as Input
import Graphics.Input.Field (Content, Selection, Direction(..))

-- Model

data Command =
  Start |
  Edit

type Page = {
  name:String,
  content:String,
  editing:Maybe Content
  }

type State = Page

commands = Input.input Start

-- Display

renderPage : Page -> Element
renderPage p = 
  case p.editing of
    Nothing ->
      asText p.name `above`
      plainText p.content |> Input.clickable commands.handle Edit
    Just c ->
      asText p.name `above`
      plainText p.content |> color red

-- Update

step : Command -> State -> State
step c s = case c of
  Start -> s
  Edit -> { s | editing <- Just {string=s.content, selection=Selection 0 0 Forward} }

-- Signals

initial = {name="Welcome", content="fill me", editing=Nothing}
state = foldp step initial commands.signal

main = renderPage <~ state