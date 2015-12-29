module FrameNavigator.Component
  ( init
  , view
  , update
  , Action
  , Model
  , maybeError
  , maybeFrame
  ) where


import Effects
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Signal
import Task


import HttpUtil exposing (errorToString)
import Model.Frame exposing (Frame)


type Action
  = AskForFrame (Maybe String)
  | ReceiveFrame Frame String
  | ReceiveError String


type LoadedFrame
  = Loading
  | LoadError String
  | Loaded Frame String 


type alias Model =
  { loadedFrame : LoadedFrame
  , directionId : Int
  }


maybeError : Model -> Maybe String
maybeError model =
  case model.loadedFrame of
    LoadError error -> Just error
    _ -> Nothing


maybeFrame : Model -> Maybe Frame 
maybeFrame model =
  case model.loadedFrame of
    Loaded frame _ -> Just frame 
    _ -> Nothing


maybePaginationNext : Model -> Maybe String
maybePaginationNext model =
  case model.loadedFrame of
    Loaded _ paginationNext -> Just paginationNext
    _ -> Nothing


init : Int -> (Model, Effects.Effects Action)
init directionId =
  ( { loadedFrame = Loading, directionId = directionId }
  , getNextFrame directionId Nothing
  )


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ class "col-md-12"
    ]
    [ div
        [ class "btn-group"
        ]
        [ button
            [ class "btn btn-default"
            , onClick address (AskForFrame Nothing)
            ]
            [ text "First Frame"
            ]
        , button
            [ class "btn btn-primary"
            , onClick address (AskForFrame (maybePaginationNext model)) 
            ]
            [ text "Next Frame"
            ]
        ]
    ]


update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    AskForFrame paginationNext ->
      (model, getNextFrame model.directionId paginationNext)
    ReceiveFrame frame paginationNext ->
      ({ model | loadedFrame = Loaded frame paginationNext }, Effects.none)
    ReceiveError error ->
      ({ model | loadedFrame = LoadError error }, Effects.none)


-- Data requesting.
getNextFrame : Int -> Maybe String -> Effects.Effects Action 
getNextFrame directionId nextFrame =
  let
    startQueryParam = case nextFrame of
      Just justNextFrame ->
        "&start=" ++ justNextFrame
      Nothing ->
        ""
  in
    Http.get
      Model.Frame.responseDecoder
      ("/frames.v1?q=direction&directionId=" ++ (toString directionId) ++ "&limit=1" ++ startQueryParam)
      |> Task.toResult
      |> Task.map interpretFrameResponse
      |> Effects.task


interpretFrameResponse : Result Http.Error Model.Frame.FrameResponse -> Action 
interpretFrameResponse maybeFrameResponse =
  case maybeFrameResponse of
    Ok frameResponse ->
      case List.head frameResponse.elements of
        Just frame -> ReceiveFrame frame frameResponse.paginationNext
        Nothing -> ReceiveError "Response has no frames in it."
    Err error ->
      ReceiveError (errorToString error)
