import Debug
import Effects
import Json.Decode as D
import Json.Encode as E
import Html exposing (..)
import Html.Events exposing (..)
import Http
import List
import Result exposing (..)
import Signal
import StartApp
import Svg
import Svg.Attributes exposing (..)
import Task


import AutoSave
import HttpUtil exposing (errorToString)
import Model.DirectionAnnotation as DirectionAnnotation
import Model.Frame
import RegionsEditor


directionId : Int
directionId = 3


ourHeight : Int
ourHeight = 240 * 2


ourWidth : Int
ourWidth = 352 * 2


type Action
  = RegionsEditorAction RegionsEditor.Action
  | AskForFrame (Maybe String)
  | ReceiveNextFrame Model.Frame.Frame String
  | ReceiveError String
  | AutoSaveAction AutoSave.Action


type LoadedFrame
  = NotLoading
  | Loading
  | Loaded Model.Frame.Frame String


type alias Model =
  { loadedFrame : LoadedFrame
  , regionsEditorModel : RegionsEditor.Model
  , autoSave: AutoSave.Model DirectionAnnotation.DirectionAnnotation
  }


maybeNextFrame : Model -> Maybe String
maybeNextFrame model =
  case model.loadedFrame of
    Loaded _ nextFrame -> Just nextFrame
    _ -> Nothing

init : (Model, Effects.Effects Action)
init =
  let
    (autoSaveModel, autoSaveEffects) = AutoSave.init saveDirectionAnnotation
  in
    ( { loadedFrame = Loading
      , regionsEditorModel = RegionsEditor.init
      , autoSave = autoSaveModel 
      }
    , batch
      [ getNextFrame Nothing
      , Effects.map AutoSaveAction autoSaveEffects
      ]
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


navigationButtons : Signal.Address Action -> Model -> Html
navigationButtons address model =
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
            , onClick address (AskForFrame (maybeNextFrame model)) 
            ]
            [ text "Next Frame"
            ]
        ]
    ]


editorButtons : Signal.Address Action -> Model -> Html
editorButtons address model =
  div
    [ class "col-md-12"
    ]
    [ RegionsEditor.buttons (Signal.forwardTo address RegionsEditorAction) model.regionsEditorModel
    ]


sideBar : Signal.Address Action -> Model -> Html
sideBar address model =
  div
    []
    [ div
        [ class "row"
        ]
        [ AutoSave.view model.autoSave
        ]
    , div
        [ class "row"
        ]
        [ editorButtons address model
        ]
    , div
        [ class "row"
        ]
        [ navigationButtons address model
        ]
    ]   


frameView : Signal.Address Action -> Model.Frame.Frame -> Model -> Html
frameView address frame model =
  div
    [ class "row"
    ]
    [ div
        [ class "col-md-8"
        ]
        [ editorWithImage address frame model
        ]
    , div
        [ class "col-md-4"
        ]
        [ sideBar address model
        ]
    ]


rootView : Html -> Html
rootView html =
  div
    [ class "container"
    ]
    [ html
    ]


view : Signal.Address Action -> Model -> Html
view address model =
  case model.loadedFrame of
    NotLoading ->
      rootView (text "Nothing is loaded")
    Loading ->
      rootView (text "Loading...")
    Loaded frame _ ->
      rootView (frameView address frame model)


update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case (Debug.log "action" action) of
    RegionsEditorAction regionsEditorAction ->
      ( { model | regionsEditorModel = RegionsEditor.update regionsEditorAction model.regionsEditorModel }
      , Effects.none
      )
    AskForFrame nextFrame -> (model, getNextFrame nextFrame)
    ReceiveNextFrame frame nextFrame -> ({ model | loadedFrame = Loaded frame nextFrame }, Effects.none)
    ReceiveError _ -> (model, Effects.none)
    AutoSaveAction autoSaveAction ->
      let
        (autoSaveModel, autoSaveEffects) = AutoSave.update autoSaveAction model.autoSave
      in
        ({ model | autoSave = autoSaveModel }, Effects.map AutoSaveAction autoSaveEffects)


getNextFrame : Maybe String -> Effects.Effects Action
getNextFrame nextFrame =
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
        Just frame -> ReceiveNextFrame frame frameResponse.paginationNext
        Nothing -> ReceiveError "Response has no frames in it."
    Err error ->
      ReceiveError (errorToString error)


saveDirectionAnnotation : DirectionAnnotation -> Task.Task Task.Never (Result String ())
saveDirectionAnnotation data =
  Http.send
    Http.defaultSettings
    { verb = "PUT"
    , url = ("directions.v1/" ++ (toString directionId))
    , body = Http.string E.encode 0 ((E.object [("annotations", E.encode 0 (DirectionAnnotation.encode data))]))
    }
    |> Http.fromJson D.value
    |> Task.map (formatError errorToString)


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
