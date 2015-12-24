module RegionEditor (..) where


import List
import Signal
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)


import MouseEvents exposing (..)
import Path exposing (Path, addPoint, close, setPointPosition, svgPathString)


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
  , path : Path
  }


init : Path -> Model
init path = 
  { state = Free
  , path = path 
  }


type Action
  = MouseDown (Int, Int)
  | MouseMove (Int, Int)
  | MouseUp (Int, Int)


getHoverIndex : (Int, Int) -> Path -> Maybe Int
getHoverIndex (x, y) path =
  let
    distances = List.indexedMap (\i (px, py) -> (i, (px-x)*(px-x)+(py-y)*(py-y))) path.points
    eligibleDistances = List.filter (\(_, d) -> d <= pointRadius*pointRadius) distances
    sortedEligibleDistances = List.sortBy (\(_, d) -> d) eligibleDistances
  in
    List.head <| List.map (\(i, _) -> i) sortedEligibleDistances



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
          { model | path = setPointPosition index (x, y) model.path }
        _ ->
          case getHoverIndex (x, y) model.path of
            Just hoverIndex ->
              { model | state = Hovering hoverIndex }
            Nothing ->
              { model | state = Free }
    MouseUp (x, y) ->
      case model.state of
        Dragging index ->
          if index == List.length model.path.points - 1
             then { model | path = close model.path, state = Hovering index }
             else { model | state = Hovering index }
        Free ->
          if model.path.closed
            then model
            else { model | path = addPoint (x, y) model.path, state = Hovering 0 } 
        _ ->
          model


lines : Model -> Svg
lines model =
  Svg.path
    [ d (svgPathString model.path) 
    , fill "none"
    , stroke "pink"
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
    , stroke (if active then "blue" else "pink")
    , strokeWidth "2"
    ]
    []


points : Signal.Address Action -> Model -> Svg
points address model =
  g
    []
    (List.indexedMap (\i p -> point address i (Just i == activeIndex model.state) p) model.path.points)


view : Signal.Address Action -> Model -> Svg 
view address model =
  g
    []
    [ lines model
    , points address model
    , rect
        [ width "500"
        , height "500"
        , pointerEvents "visible"
        , stroke "none"
        , fill "none"
        , onMouseDown (\v -> Signal.message address (MouseDown v))
        , onMouseMove (\v -> Signal.message address (MouseMove v))
        , onMouseUp (\v -> Signal.message address (MouseUp v))
        ]
        []
    ]
