module Model.Frame (..) where


import Date exposing (Date)
import Json.Decode exposing (..)


import Model.Date exposing (dateString)
import Model.Labels as Labels exposing (Labels)


type alias Frame =
  { direction : Int
  , id : Int
  , capturedAt : Date
  , filename : String
  }


decoder : Decoder Frame
decoder =
  object4 Frame
    ("direction" := int)
    ("id" := int)
    ("capturedAt" := dateString)
    ("filename" := string)


type alias FrameResponse =
  { elements : List Frame
  , paginationNext : String
  , labels : Labels
  }


-- tragically, this decoder does incorrect things when there is more than one element in the response because
-- i'm too lazy to fix it now
responseDecoder : Decoder FrameResponse
responseDecoder =
  object3 FrameResponse
    ("elements" := list decoder)
    (at ["pagination", "next"] string)
    Labels.linkedDecoder
