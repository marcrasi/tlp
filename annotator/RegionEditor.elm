module RegionEditor (..) where

import Color exposing (..)
import Debug
import Graphics.Collage exposing (..)
import List exposing (..)
import Transform2D exposing (..)

type State
  = Free
  | Hovering Int
  | Moving Int

activeIndex : State -> Maybe Int
activeIndex state =
  case state of
    Free -> Nothing
    Hovering index -> Just index
    Moving index -> Just index

type alias Model =
  { state : State
  , points : List (Int, Int)
  , closed : Bool
  }

floatedPoints : Model -> List (Float, Float)
floatedPoints model =
  map (\(x, y) -> (toFloat x, toFloat y)) model.points

hoveredIndex : Int -> Int -> Model -> Maybe Int
hoveredIndex x y model =
  let
    fx = toFloat x
    fy = toFloat y
    distances = map (\(px, py) -> (fx-px)*(fx-px)+(fy-py)*(fy-py)) (floatedPoints model) 
    indexedDistances = indexedMap (,) distances
    eligibleDistances = filter (\(_, d) -> d < 25.0) indexedDistances
    sortedEligibleDistances = sortBy (\(_, d) -> d) eligibleDistances
  in
    head <| map (\(i, _) -> i) sortedEligibleDistances

type Action
  = MouseDown Int Int
  | MouseUp Int Int
  | MouseMove Int Int

update : Action -> Model -> Model 
update action model =
  model
    |> updateClicks action
    |> updateDrags action
    |> updateHover action

updateClicks : Action -> Model -> Model
updateClicks action model =
  case model.state of
    Free ->
      case action of
        MouseUp x y -> { model | points = (x, y) :: model.points, state = Hovering 0 }
        _ -> model
    Hovering index ->
      case action of
        MouseDown x y -> { model | state = Moving index }
        _ -> model
    Moving index ->
      case action of
        MouseUp x y -> 
          let
            hmodel = { model | state = Hovering index }
          in
            if index == length model.points - 1
              then { hmodel | closed = True }
              else hmodel 
        _ -> model

updateDrags : Action -> Model -> Model
updateDrags action model =
  case model.state of
    Moving index ->
      case action of
        MouseMove x y ->
          let
            indexedPoints = indexedMap (,) model.points
            newPoints = map (\(i, p) -> if i == index then (x, y) else p) indexedPoints
          in
            { model | points = newPoints }
        _ -> model
    _ -> model

updateHover : Action -> Model -> Model
updateHover action model =
  case model.state of
    Free ->
      case action of
        MouseMove x y ->
          case hoveredIndex x y model of
            Just index -> { model | state = Hovering index }
            Nothing -> { model | state = Free }
        _ -> model
    Hovering _ ->
      case action of
        MouseMove x y ->
          case hoveredIndex x y model of
            Just index -> { model | state = Hovering index }
            Nothing -> { model | state = Free }
        _ -> model
    Moving _ -> model

view : Model -> Form
view model =
  groupTransform
    -- hmm, this should not need to know its width and height
    -- unless I explicitly specify them in some sort of props!
    -- which might be a good thing to do!
    (multiply (translation -250 250) (scaleY -1.0))
    [ outline model
    , handles model
    ]

handles : Model -> Form
handles model =
  let
    indexedPoints = indexedMap (,) (floatedPoints model)
    pointsWithActive = map (\(index, point) -> (point, (activeIndex model.state) == Just index)) indexedPoints 
  in
    group <| map (\(point, active) -> handle point active) pointsWithActive 

handle : (Float, Float) -> Bool -> Form
handle point active =
  let
    lineStyle = if active then solid green else solid red
  in
    move point <| outlined lineStyle (circle 5.0) 

outline : Model -> Form
outline model =
  let
    fps = floatedPoints model
    maybeClosed = if model.closed
      then append fps (take 1 fps)
      else fps
  in
    traced (solid red) (path maybeClosed) 
