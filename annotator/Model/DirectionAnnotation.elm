module Model.DirectionAnnotation (..) where


import Json.Decode exposing (..)
import Json.Encode as E
import List


import Model.Path as Path


type alias DirectionAnnotation =
  { regions : List Path.Path 
  }


decoder : Decoder DirectionAnnotation
decoder =
  object1 DirectionAnnotation ("regions" := list Path.decoder)


encode : DirectionAnnotation -> Value
encode directionAnnotation =
  E.object [("regions", E.list (List.map Path.encode directionAnnotation.regions))]
