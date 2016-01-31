module Model.Hour (Hour, HoursResponse, decoder, responseDecoder) where


import Date exposing (Date)
import Json.Decode exposing (..)


import Model.Date exposing (unixSeconds)


type alias Hour =
  { start : Date
  , frameCount : Int
  }


type alias HoursResponse =
  { elements : List Hour }


decoder : Decoder Hour
decoder =
  object2 Hour
    ("start" := unixSeconds)
    ("frameCount" := int)


responseDecoder : Decoder HoursResponse
responseDecoder =
  object1 HoursResponse
    ("elements" := list decoder)
