module RegionEditor (..) where


import Html
import List
import Signal
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)


import MouseEvents exposing (..)


pointRadius : Int
pointRadius = 5


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
  = MouseDown (Int, Int)
  | MouseMove (Int, Int)
  | MouseUp (Int, Int)


getHoverIndex : (Int, Int) -> List (Int, Int) -> Maybe Int
getHoverIndex (x, y) points =
  let
    distances = List.indexedMap (\i (px, py) -> (i, (px-x)*(px-x)+(py-y)*(py-y))) points
    eligibleDistances = List.filter (\(_, d) -> d <= pointRadius*pointRadius) distances
    sortedEligibleDistances = List.sortBy (\(_, d) -> d) eligibleDistances
  in
    List.head <| List.map (\(i, _) -> i) sortedEligibleDistances


setPointPosition : Int -> (Int, Int) -> Model -> Model
setPointPosition index p model =
  { model | points = List.indexedMap (\i x -> if i == index then p else x) model.points }


update : Action -> Model -> Model
update action model =
  case action of
    MouseDown (x, y) ->
      case model.state of
        Hovering index ->
          { model | state = Dragging index }
        _ ->
          model
    MouseMove (x, y) ->
      case model.state of
        Dragging index ->
          setPointPosition index (x, y) model
        _ ->
          case getHoverIndex (x, y) model.points of
            Just hoverIndex ->
              { model | state = Hovering hoverIndex }
            Nothing ->
              { model | state = Free }
    MouseUp (x, y) ->
      case model.state of
        Dragging index ->
          if index == List.length model.points - 1
             then { model | closed = True, state = Hovering index }
             else { model | state = Hovering index }
        Free ->
          if model.closed
            then model
            else { model | points = (x, y) :: model.points, state = Hovering 0 }
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
  let
    notClosedPathString = List.foldl addPointToPathString "" model.points
    maybeClosedPathString = if model.closed
      then closePathString notClosedPathString
      else notClosedPathString
  in
    Svg.path
      [ d maybeClosedPathString 
      , fill "none"
      , stroke "black"
      , strokeWidth "2"
      ]
      []


point : Signal.Address Action -> Int -> Bool -> (Int, Int) -> Svg 
point address index active (x, y) = 
  circle
    [ cx (toString x)
    , cy (toString y)
    , r "5"
    , fill "none"
    , stroke (if active then "blue" else "black")
    , strokeWidth "2"
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
    , onMouseDown (\v -> Signal.message address (MouseDown v))
    , onMouseMove (\v -> Signal.message address (MouseMove v))
    , onMouseUp (\v -> Signal.message address (MouseUp v))
    ]
    [ lines model, points address model ]
