module Model.Direction (..) where


import Json.Decode exposing (..)
import List
import Result exposing (Result(Ok, Err))

import Model.DirectionAnnotation as DA


type alias Direction =
  { annotation : DA.DirectionAnnotation
  }


decoder : Decoder Direction
decoder =
  object1 Direction
    ("annotations" := string `andThen` stringAnnotationDecoder)


stringAnnotationDecoder : String -> Decoder DA.DirectionAnnotation
stringAnnotationDecoder value =
  if value == ""
    then
      succeed (DA.DirectionAnnotation [])
    else
      case decodeString DA.decoder value of
        Ok annotation ->
          succeed annotation
        Err error ->
          fail error


responseDecoder : Decoder Direction
responseDecoder =
  "elements" := listHead (list decoder)


listHead : Decoder (List a) -> Decoder a
listHead decoder =
  decoder `andThen` (\l ->
    case List.head l of
      Just h -> succeed h
      Nothing -> fail "expecting non-empty list")
