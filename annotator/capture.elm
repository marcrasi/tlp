import Effects
import Http
import Html exposing (Html)
import Json.Encode as E
import List
import Result
import Signal
import StartApp
import Svg
import Svg.Attributes exposing (..)
import Task
import Time exposing (Time)


import Constants exposing (directionId, ourHeight, ourWidth)
import HttpUtil exposing (errorToString)
import Model.Frame exposing (Frame)
import Model.FrameRequest as FR


updatePeriod : Time
updatePeriod = 1 * Time.second


type Action
  = ReceiveFrame Frame Time
  | ReceiveError String
  | Tick Time


type FrameLoadStatus
  = NoFrame
  | LoadedFrame Frame Time
  | LoadingFrameHaveOld Frame


maybeFrame : FrameLoadStatus -> Maybe Frame
maybeFrame fls =
  case fls of
    NoFrame -> Nothing
    LoadedFrame frame _ -> Just frame
    LoadingFrameHaveOld frame -> Just frame


type alias Model =
  { frameLoadStatus : FrameLoadStatus
  }


init : (Model, Effects.Effects Action)
init =
  ( { frameLoadStatus = NoFrame }
  , Effects.tick Tick
  )


view : Signal.Address Action -> Model -> Html
view address model =
  case maybeFrame model.frameLoadStatus of
    Nothing -> Html.text "Loading first frame..."
    Just frame ->
      viewFrame address frame


viewFrame : Signal.Address Action -> Frame -> Html
viewFrame address frame =
  Svg.svg
    [ width (toString ourWidth)
    , height (toString ourHeight)
    ]
    [ Svg.image
      [ xlinkHref ("/" ++ frame.filename)
      , x "0"
      , y "0"
      , width (toString ourWidth)
      , height (toString ourHeight)
      ]
      []
    ]


update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    ReceiveFrame frame requestedAt ->
      ( { model | frameLoadStatus = LoadedFrame frame requestedAt }
      , Effects.none
      )
    ReceiveError error -> (model, Effects.none)
    Tick time ->
      case model.frameLoadStatus of
        NoFrame ->
          ( model
          , Effects.batch
            [ Effects.tick Tick
            , requestFrame time
            ]
          )
        LoadedFrame frame requestedAt ->
          if (time - requestedAt) >= updatePeriod
            then
              ( { model | frameLoadStatus = LoadingFrameHaveOld frame }
              , Effects.batch
                [ Effects.tick Tick
                , requestFrame time
                ]
              )
            else
              (model, Effects.tick Tick)
        LoadingFrameHaveOld _ ->
          (model, Effects.tick Tick)


requestFrame : Time -> Effects.Effects Action
requestFrame requestedAt =
  Http.send
    Http.defaultSettings
    { verb = "POST"
    , headers = []
    , url = "/frameRequests.v1"
    , body = Http.string (E.encode 0 (FR.encodeFrameRequestCreate (FR.FrameRequestCreate directionId)))
    }
    |> Http.fromJson FR.decoder
    |> Task.toResult
    |> Task.map (interpretFrameRequestResponse requestedAt)
    |> Effects.task


interpretFrameRequestResponse : Time -> Result Http.Error FR.FrameRequestResponse -> Action
interpretFrameRequestResponse requestedAt result =
  case result of
    Ok frameRequestResponse -> ReceiveFrame frameRequestResponse.frame requestedAt
    Err error -> ReceiveError (errorToString error)


app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = []
    }


main = app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks = app.tasks
