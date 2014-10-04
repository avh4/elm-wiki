import Graphics.Input as Input
import Graphics.Input.Field as Field
import Graphics.Input.Field (Content, Selection, Direction(..))
import Window
import Http

-- Model

data Command =
  Start |
  Edit Int | Update Int Content | CommitAllEdits |
  Add |
  Loaded String

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

pageWidth = 300
pageMargin = 20

renderPage : Int -> Page -> Element
renderPage i p =
  width (pageWidth + pageMargin) <| flow down [width pageWidth <| color blue <|
  case p.editing of
    Nothing ->
      asText p.name `above`
      (plainText p.content |> width pageWidth) |> Input.clickable commands.handle (Edit i)
    Just c ->
      asText p.name `above`
      Field.field Field.defaultStyle commands.handle (\u -> Update i u) "" c |> Input.clickable ignore ()
      ]

renderPages : [Page] -> Element
renderPages pages = flow right <| indexedMap renderPage pages

renderScreen : (Int,Int) -> State -> Element
renderScreen (w,h) s =
  (renderPages s.pages
  `below` Input.button commands.handle Add "Add Page")
  |> height h |> Input.clickable commands.handle CommitAllEdits

-- Update

mapAt : Int -> (a -> a) -> [a] -> [a]
mapAt n fn list = indexedMap (\i -> \o -> if i == n then fn o else o) list

newPage s = {name="Page", content=s, editing=Nothing}

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
  Add -> { s | pages <- s.pages ++ [newPage ""]}
  Loaded content -> { s | pages <- s.pages ++ [newPage content]}

-- Signals

httpToLoadCommand res = case res of
  Http.Success s -> Loaded s
  _ -> Start

contentLoad : Signal Command
contentLoad = httpToLoadCommand <~ Http.send (constant (Http.request "GET" "https://api.github.com/repos/avh4/wiki/contents/blogs.md" "" [("Accept", "application/vnd.github.VERSION.raw")]))

initial = {pages=[
  {name="Welcome", content="fill me", editing=Nothing},
  {name="More info", content="fill me 2", editing=Nothing}
  ]}
state = foldp step initial (merge commands.signal contentLoad)

main = renderScreen <~ Window.dimensions ~ state