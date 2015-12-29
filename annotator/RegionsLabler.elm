module RegionsLabler (..) where


import Html
import Svg
import Svg.Attributes exposing (..)
import Svg.Events


import Model.DirectionAnnotation as DirectionAnnotation exposing (DirectionAnnotation)
import Model.Path exposing (Path, svgPathString)


type alias Model =
  { annotation : DirectionAnnotation
  }


init : DirectionAnnotation -> Model
init annotation =
  { annotation = annotation }


region : Path -> Svg.Svg
region path =
  Svg.path
    [ d (svgPathString path)
    , fill "none"
    , stroke "red"
    , strokeWidth "2"
    ]
    []


view : Model -> Svg.Svg
view model =
  Svg.g
    []
    (model.annotation.regions
      |> List.map (.path)
      |> List.map region)
