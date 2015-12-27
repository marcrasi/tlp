module Model.Frame (..) where


import Json.Decode exposing (..)


type alias Frame =
  { direction : Int
  , id : Int
  , capturedAt : String
  , filename : String
  }


decoder : Decoder Frame
decoder =
  object4 Frame
    ("direction" := int)
    ("id" := int)
    ("capturedAt" := string)
    ("filename" := string)


type alias FrameResponse =
  { elements : List Frame
  , paginationNext : String
  }


responseDecoder : Decoder FrameResponse
responseDecoder =
  object2 FrameResponse
    ("elements" := list decoder)
    (at ["pagination", "next"] string)
