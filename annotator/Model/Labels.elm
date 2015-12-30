module Model.Labels (..) where


import Json.Decode exposing (..)
import Json.Encode as E
import List


type alias Label =
  { regionId : Int
  , value : String
  }


encodeLabel : Label -> Value
encodeLabel label =
  E.object
    [ ("regionId", E.int label.regionId)
    , ("value", E.string label.value)
    ]


labelDecoder : Decoder Label
labelDecoder =
  object2 Label
    ("region" := int)
    ("value" := string)


type alias Labels =
  { labels : List Label
  }


encodeLabels : Labels -> Value
encodeLabels labels =
  E.object [("labels", E.list (List.map encodeLabel labels.labels))]


linkedDecoder : Decoder Labels
linkedDecoder =
  object1 Labels
    (at ["linked", "labels.v1"] (list labelDecoder))
