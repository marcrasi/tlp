module Model.DirectionAnnotation (..) where


import Json.Decode exposing (..)
import Json.Encode as E
import List


import Model.Path as Path


type alias Region =
  { id : Maybe Int
  , path : Path.Path
  }


valueDecoder : Decoder Path.Path
valueDecoder =
  string `andThen` (\s -> case decodeString Path.decoder s of
    Ok path -> succeed path
    Err error -> fail error)


valueEncode : Path.Path -> String
valueEncode path =
  E.encode 0 (Path.encode path)


regionDecoder : Decoder Region
regionDecoder =
  object2 Region
    ("id" := maybe int)
    ("value" := valueDecoder) 


regionEncode : Region -> Value
regionEncode region =
  E.object
    [ ("id", Maybe.withDefault E.null (Maybe.map E.int region.id))
    , ("value", E.string (valueEncode region.path))
    ]


type alias DirectionAnnotation =
  { regions : List Region 
  }


decodeDirectionResponse : Decoder DirectionAnnotation
decodeDirectionResponse =
  object1 DirectionAnnotation
    (at ["linked", "regions.v1"] (list regionDecoder))


encodeDirectionUpdate : DirectionAnnotation -> Value
encodeDirectionUpdate directionAnnotation =
  E.object [("regions", E.list (List.map regionEncode directionAnnotation.regions))]
