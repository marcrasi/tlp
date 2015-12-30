module RegionsLabler (..) where


import Html
import Signal
import Svg
import Svg.Attributes exposing (..)
import Svg.Events


import ListUtil
import Model.DirectionAnnotation as DirectionAnnotation exposing (DirectionAnnotation, Region)
import Model.Labels as Labels exposing (Labels)
import Model.Path exposing (Path, svgPathString)


type Action
  = SetLabel Int (Maybe String)


type alias Model =
  { annotation : DirectionAnnotation
  , labels : Labels
  }


init : DirectionAnnotation -> Model
init annotation =
  { annotation = annotation
  , labels = Labels []
  }


updateLabel : Int -> Maybe String -> Labels -> Labels
updateLabel regionId value labels =
  let
    withoutThisRegion = List.filter (\x -> x.regionId /= regionId) labels.labels
  in
    case value of
      Just v ->
        { labels | labels = { regionId = regionId, value = v } :: withoutThisRegion }
      Nothing ->
        { labels | labels = withoutThisRegion }


update : Action -> Model -> Model
update action model =
  case action of
    SetLabel regionId value ->
      { model | labels = updateLabel regionId value model.labels }


nextValue : Maybe String -> Maybe String
nextValue value =
  case value of
    Nothing -> Just "occupied"
    Just "occupied" -> Just "unoccupied"
    Just "unoccupied" -> Nothing
    _ -> Nothing


valueColor : Maybe String -> String 
valueColor value =
  case value of
    Nothing -> "white" 
    Just "occupied" -> "red" 
    Just "unoccupied" -> "green" 
    _ -> "white"


region : Signal.Address Action -> Model -> Region -> Svg.Svg
region address model region =
  case region.id of
    Just regionId ->
      let
        value = model.labels.labels
          |> List.filter (\x -> x.regionId == regionId)
          |> List.head
          |> Maybe.map (.value)
      in
        Svg.path
          [ d (svgPathString region.path)
          , fill "none"
          , stroke (valueColor value) 
          , strokeWidth "2"
          , pointerEvents "visible"
          , Svg.Events.onClick (Signal.message address (SetLabel regionId (nextValue value)))
          ]
          []
    Nothing -> Svg.text "This is not good."


view : Signal.Address Action -> Model -> Svg.Svg
view address model =
  Svg.g
    []
    (model.annotation.regions
      |> List.map (region address model))
