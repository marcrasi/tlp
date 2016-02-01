module Model.FrameRequest (..) where


import Json.Decode exposing (..)
import Json.Encode as E
import List


import Model.Frame as F exposing (Frame)

type alias FrameRequestCreate =
  { directionId : Int }


encodeFrameRequestCreate : FrameRequestCreate -> Value
encodeFrameRequestCreate frameRequestCreate =
  E.object
    [ ("directionId", E.int frameRequestCreate.directionId) ]


type alias FrameRequestResponse =
  { isNew : Bool
  , frame : Frame
  }


decoder : Decoder FrameRequestResponse
decoder =
  object2 FrameRequestResponse
    ("elements" := (listHead ("isNew" := bool)))
    (at ["linked", "frames.v1"] (listHead F.decoder))


listHead : Decoder a -> Decoder a
listHead underlying =
  (list underlying) `andThen` (\l ->
    case List.head l of
      Just h -> succeed h
      Nothing -> fail "Expected non-empty list")
