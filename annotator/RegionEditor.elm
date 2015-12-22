module RegionEditor (..) where


import Html
import List
import Signal
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)


import MouseEvents exposing (..)


type State
  = Free
  | Hovering Int
  | Dragging Int


activeIndex : State -> Maybe Int
activeIndex state =
  case state of
    Free -> Nothing
    Hovering index -> Just index
    Dragging index -> Just index


type alias Model =
  { state : State
  , points : List (Int, Int)
  , closed : Bool
  }


type Action
  = ClickPoint Int
  | ClickCanvas (Int, Int)
  | EnterPoint Int
  | ExitPoint Int
  | MouseDownPoint Int
  | MouseUpPoint Int
  | MouseMoveCanvas (Int, Int)


update : Action -> Model -> Model
update action model =
  case action of
    ClickPoint index ->
      if index == List.length model.points - 1
         then { model | closed = True }
         else model
    ClickCanvas (x, y) ->
      { model | points = (x, y) :: model.points, state = Hovering 0 }
    EnterPoint index ->
      { model | state = Hovering index }
    ExitPoint index ->
      { model | state = Free }
    MouseDownPoint index ->
      { model | state = Dragging index }
    MouseUpPoint index ->
      { model | state = Hovering index }
    MouseMoveCanvas (x, y) ->
      case model.state of
        Dragging index ->
          let
            indexedPoints = List.indexedMap (,) model.points
            newPoints = List.map (\(i, p) -> if i == index then (x, y) else p) indexedPoints
          in
            { model | points = newPoints }
        _ ->
          model

addPointToPathString : (Int, Int) -> String -> String
addPointToPathString (x, y) pathString =
  let
    command = if String.length pathString == 0 then "M" else "L"
  in
    pathString ++ " " ++ command ++ " " ++ toString x ++ " " ++ toString y


closePathString : String -> String
closePathString pathString = pathString ++ " Z"


lines : Model -> Svg
lines model =
  Svg.path
    [ d (List.foldl addPointToPathString "" model.points)
    , fill "none"
    , stroke "black"
    , strokeWidth "1"
    ]
    []


point : Signal.Address Action -> Int -> Bool -> (Int, Int) -> Svg 
point address index active (x, y) = 
  circle
    [ cx (toString x)
    , cy (toString y)
    , r "5"
    , fill "black"
    , stroke (if active then "blue" else "black")
    , strokeWidth "1"
    , onClick (always (Signal.message address (ClickPoint index)))
    , onMouseOver (always (Signal.message address (EnterPoint index)))
    , onMouseOut (always (Signal.message address (ExitPoint index)))
    , onMouseDown (always (Signal.message address (MouseDownPoint index)))
    , onMouseUp (always (Signal.message address (MouseUpPoint index)))
    ]
    []


points : Signal.Address Action -> Model -> Svg
points address model =
  g
    []
    (List.indexedMap (\i p -> point address i (Just i == activeIndex model.state) p) model.points)


view : Signal.Address Action -> Model -> Html.Html
view address model =
  svg
    [ width "500"
    , height "500"
    , onClick (\v -> Signal.message address (ClickCanvas v))
    , onMouseMove (\v -> Signal.message address (MouseMoveCanvas v))
    ]
    [ lines model, points address model ]
