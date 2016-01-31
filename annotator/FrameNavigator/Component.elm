module FrameNavigator.Component
  ( init
  , view
  , update
  , Action
  , Model
  , maybeError
  , maybeFrame
  ) where


import Date exposing (Date, toTime)
import Date.Format exposing (format)
import Effects
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Signal
import Task


import HttpUtil exposing (errorToString)
import Model.Frame exposing (Frame)
import Model.Labels as Labels exposing (Labels)
import Model.Hour exposing (Hour, responseDecoder)


type Action
  = AskForFrame (Maybe String)
  | ReceiveFrame Frame Labels String
  | ReceiveFrameError String
  | ReceiveHours (List Hour)
  | ReceiveHoursError String


type FrameLoadStatus
  = LoadingFrame
  | LoadFrameError String
  | LoadedFrame Frame Labels String


type HoursLoadStatus
  = LoadingHours
  | LoadHoursError String
  | LoadedHours (List Hour)


type alias Model =
  { frameLoadStatus : FrameLoadStatus
  , hoursLoadStatus : HoursLoadStatus
  , directionId : Int
  }


maybeError : Model -> Maybe String
maybeError model =
  case model.frameLoadStatus of
    LoadFrameError error -> Just error
    _ -> Nothing


maybeFrame : Model -> Maybe (Frame, Labels)
maybeFrame model =
  case model.frameLoadStatus of
    LoadedFrame frame labels _ -> Just (frame, labels)
    _ -> Nothing


maybePaginationNext : Model -> Maybe String
maybePaginationNext model =
  case model.frameLoadStatus of
    LoadedFrame _ _ paginationNext -> Just paginationNext
    _ -> Nothing


init : Int -> (Model, Effects.Effects Action)
init directionId =
  ( { frameLoadStatus = LoadingFrame, hoursLoadStatus = LoadingHours, directionId = directionId }
  , Effects.batch [getNextFrame directionId Nothing, getHours]
  )


dateFormat : Date -> String
dateFormat = format "%Y-%m-%d %H:%M:%S"


viewHour : Signal.Address Action -> Hour -> Html
viewHour address hour =
  li
    [ onClick address (AskForFrame (Just (toString (toTime hour.start / 1000))))
    ]
    [ text ((dateFormat hour.start) ++ ": " ++ (toString hour.frameCount))
    ]


viewHours : Signal.Address Action -> Model -> Html
viewHours address model =
  case model.hoursLoadStatus of
    LoadingHours -> text "Loading Hours..."
    LoadedHours hours ->
      ul
        []
        (List.map (viewHour address) hours)
    LoadHoursError error -> text ("Error loading hours: " ++ error)


viewButtons : Signal.Address Action -> Model -> Html
viewButtons address model =
  div
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


viewFrameTime : Model -> Html
viewFrameTime model =
  case maybeFrame model of
    Just (frame, _) -> text (dateFormat frame.capturedAt)
    Nothing -> text ""



view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ class "col-md-12"
    ]
    [ div [] [viewFrameTime model]
    , div [] [viewButtons address model]
    , div [] [viewHours address model]
    ]


update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    AskForFrame paginationNext ->
      (model, getNextFrame model.directionId paginationNext)
    ReceiveFrame frame labels paginationNext ->
      ({ model | frameLoadStatus = LoadedFrame frame labels paginationNext }, Effects.none)
    ReceiveFrameError error ->
      ({ model | frameLoadStatus = LoadFrameError error }, Effects.none)
    ReceiveHours hours ->
      ({ model | hoursLoadStatus = LoadedHours hours }, Effects.none)
    ReceiveHoursError error ->
      ({ model | hoursLoadStatus = LoadHoursError error }, Effects.none)


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
        Just frame -> ReceiveFrame frame frameResponse.labels frameResponse.paginationNext
        Nothing -> ReceiveFrameError "Response has no frames in it."
    Err error ->
      ReceiveFrameError (errorToString error)


getHours : Effects.Effects Action
getHours =
  Http.get
    Model.Hour.responseDecoder
    "/hours.v1"
    |> Task.toResult
    |> Task.map interpretHoursResponse
    |> Effects.task


interpretHoursResponse : Result Http.Error Model.Hour.HoursResponse -> Action
interpretHoursResponse maybeHoursResponse =
  case maybeHoursResponse of
    Ok hoursResponse -> ReceiveHours hoursResponse.elements
    Err error -> ReceiveHoursError (errorToString error)
