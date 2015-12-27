import Debug
import Effects
import Html exposing (..)
import Http
import List
import Result exposing (..)
import Signal
import StartApp
import Svg
import Svg.Attributes exposing (..)
import Task


import Model.Frame
import RegionsEditor


ourHeight : Int
ourHeight = 240 * 2


ourWidth : Int
ourWidth = 352 * 2


type Action
  = RegionsEditorAction RegionsEditor.Action
  | NextFrame
  | ReceiveNextFrame Model.Frame.Frame String
  | ReceiveError String


type LoadedFrame
  = NotLoading
  | Loading
  | Loaded Model.Frame.Frame String


type alias Model =
  { loadedFrame : LoadedFrame
  , regionsEditorModel : RegionsEditor.Model
  }


maybeNextFrame : Model -> Maybe String
maybeNextFrame model =
  case model.loadedFrame of
    Loaded _ nextFrame -> Just nextFrame
    _ -> Nothing

init : (Model, Effects.Effects Action)
init =
  ( { loadedFrame = Loading
    , regionsEditorModel = RegionsEditor.init
    }
  , getNextFrame Nothing
  )


editorWithImage : Signal.Address Action -> Model.Frame.Frame -> Model -> Html
editorWithImage address frame model =
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
    , RegionsEditor.view (Signal.forwardTo address RegionsEditorAction) model.regionsEditorModel
    ]


buttons : Signal.Address Action -> Model -> Html
buttons address model =
  div [] []


view : Signal.Address Action -> Model -> Html
view address model =
  case model.loadedFrame of
    NotLoading ->
      text "Nothing is loaded"
    Loading ->
      text "Loading..."
    Loaded frame _ ->
      div
        []
        [ editorWithImage address frame model
        , buttons address model
        ]


update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case (Debug.log "action" action) of
    RegionsEditorAction regionsEditorAction ->
      ( { model | regionsEditorModel = RegionsEditor.update regionsEditorAction model.regionsEditorModel }
      , Effects.none
      )
    NextFrame -> ({ model | loadedFrame = Loading }, getNextFrame <| maybeNextFrame model)
    ReceiveNextFrame frame nextFrame -> ({ model | loadedFrame = Loaded frame nextFrame }, Effects.none)
    ReceiveError _ -> (model, Effects.none)


getNextFrame : Maybe String -> Effects.Effects Action
getNextFrame nextFrame =
  Http.get Model.Frame.responseDecoder "/frames.v1?q=direction&directionId=3&limit=1"
    |> Task.toResult
    |> Task.map interpretFrameResponse
    |> Effects.task


interpretFrameResponse : Result Http.Error Model.Frame.FrameResponse -> Action
interpretFrameResponse maybeFrameResponse =
  case maybeFrameResponse of
    Ok frameResponse ->
      case List.head frameResponse.elements of
        Just frame -> ReceiveNextFrame frame frameResponse.paginationNext
        Nothing -> ReceiveError "Response has no frames in it."
    Err error ->
      case error of
        Http.BadResponse _ errorString -> ReceiveError errorString
        Http.NetworkError -> ReceiveError "NetworkError"
        Http.Timeout -> ReceiveError "Timeout"
        Http.UnexpectedPayload errorString -> ReceiveError errorString


app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = []
    }


main = app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
