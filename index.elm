import Graphics.Input as Input
import Graphics.Input.Field as Field
import Graphics.Input.Field (Content, Selection, Direction(..))
import Window

-- Model

data Command =
  Start |
  Edit Int | Update Int Content | CommitAllEdits

type Page = {
  name:String,
  content:String,
  editing:Maybe Content
  }

type State = {
  pages:[Page]
  }

commands = Input.input Start

ignore = (Input.input ()).handle

-- Display

renderPage : Int -> Page -> Element
renderPage i p = 
  case p.editing of
    Nothing ->
      asText p.name `above`
      plainText p.content |> Input.clickable commands.handle (Edit i)
    Just c ->
      asText p.name `above`
      (Field.field Field.defaultStyle commands.handle (\u -> Update i u) "" c |> Input.clickable ignore ())|> color red

renderScreen : (Int,Int) -> State -> Element
renderScreen (w,h) s =
  flow down (indexedMap renderPage s.pages)
  |> container w h middle |> Input.clickable commands.handle CommitAllEdits

-- Update

mapAt : Int -> (a -> a) -> [a] -> [a]
mapAt n fn list = indexedMap (\i -> \o -> if i == n then fn o else o) list

endEditing : Page -> Page
endEditing p = case p.editing of
  Just c -> { p | content <- c.string, editing <- Nothing }
  Nothing -> p

step : Command -> State -> State
step c s = case c of
  Start -> s
  Edit i -> { s | pages <- mapAt i (\p -> { p | editing <- Just {string=p.content, selection=Selection 0 0 Forward} }) s.pages}
  Update i c -> { s | pages <- mapAt i (\p -> { p | editing <- Just c } ) s.pages }
  CommitAllEdits -> { s | pages <- map endEditing s.pages }

-- Signals

initial = {pages=[
  {name="Welcome", content="fill me", editing=Nothing},
  {name="More info", content="fill me 2", editing=Nothing}
  ]}
state = foldp step initial commands.signal

main = renderScreen <~ Window.dimensions ~ state