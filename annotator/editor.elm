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
import Model.Direction as Direction
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
  | ReceiveAnnotation DirectionAnnotation.DirectionAnnotation
  -- note: we currently have terrible error handling.
  | ReceiveError String
  | AutoSaveAction (AutoSave.Action DirectionAnnotation.DirectionAnnotation)


type LoadedFrame
  = Loading
  | Loaded Model.Frame.Frame String


type LoadedRegions
  = LoadingRegions
  | LoadedRegions RegionsEditor.Model

type alias Model =
  { loadedFrame : LoadedFrame
  , loadedRegions : LoadedRegions
  , autoSave: AutoSave.Model DirectionAnnotation.DirectionAnnotation
  , error : Maybe String
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
      , loadedRegions = LoadingRegions 
      , autoSave = autoSaveModel
      , error = Nothing
      }
    , Effects.batch
      [ getNextFrame Nothing
      , getDirection
      , Effects.map AutoSaveAction autoSaveEffects
      ]
    )


editorWithImage : Signal.Address Action -> Model.Frame.Frame -> RegionsEditor.Model -> Html
editorWithImage address frame regionsEditorModel =
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
    , RegionsEditor.view (Signal.forwardTo address RegionsEditorAction) regionsEditorModel
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


editorButtons : Signal.Address Action -> RegionsEditor.Model -> Html
editorButtons address regionsEditorModel =
  div
    [ class "col-md-12"
    ]
    [ RegionsEditor.buttons (Signal.forwardTo address RegionsEditorAction) regionsEditorModel
    ]


sideBar : Signal.Address Action -> RegionsEditor.Model -> Model -> Html
sideBar address regionsEditorModel model =
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
        [ editorButtons address regionsEditorModel
        ]
    , div
        [ class "row"
        ]
        [ navigationButtons address model 
        ]
    ]   


frameView : Signal.Address Action -> Model.Frame.Frame -> RegionsEditor.Model -> Model -> Html
frameView address frame regionsEditorModel model =
  div
    [ class "row"
    ]
    [ div
        [ class "col-md-8"
        ]
        [ editorWithImage address frame regionsEditorModel 
        ]
    , div
        [ class "col-md-4"
        ]
        [ sideBar address regionsEditorModel model
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
  case (model.loadedFrame, model.loadedRegions, model.error) of
    (_, _, Just error) ->
      rootView (text ("Error: " ++ error))
    (Loaded frame _, LoadedRegions regionsEditorModel, _) ->
      rootView (frameView address frame regionsEditorModel model)
    _ ->
      rootView (text "Loading...")


update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    RegionsEditorAction regionsEditorAction ->
      case model.loadedRegions of
        LoadingRegions ->
          (model, Effects.none)
        LoadedRegions regionsEditorModel ->
          let
            newRegionsEditorModel = RegionsEditor.update regionsEditorAction regionsEditorModel
            (autoSaveModel, autoSaveEffects) = AutoSave.update (AutoSave.DataChange { regions = newRegionsEditorModel.paths }) model.autoSave
          in
            ( { model
                | loadedRegions = LoadedRegions newRegionsEditorModel
                , autoSave = autoSaveModel
                }
            , Effects.map AutoSaveAction autoSaveEffects
            )
    AskForFrame nextFrame -> (model, getNextFrame nextFrame)
    ReceiveNextFrame frame nextFrame -> ({ model | loadedFrame = Loaded frame nextFrame }, Effects.none)
    ReceiveAnnotation annotation ->
      ({ model | loadedRegions = LoadedRegions (RegionsEditor.init annotation.regions) }, Effects.none)
    ReceiveError error -> ({ model | error = Just error }, Effects.none)
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


getDirection : Effects.Effects Action
getDirection =
  Http.get Direction.responseDecoder ("/directions.v1/" ++ (toString directionId))
    |> Task.toResult
    |> Task.map interpretDirectionResponse
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


interpretDirectionResponse : Result Http.Error Direction.Direction -> Action
interpretDirectionResponse result =
  case result of
    Ok direction ->
      ReceiveAnnotation direction.annotation
    Err error ->
      ReceiveError (errorToString error)


saveDirectionAnnotation : DirectionAnnotation.DirectionAnnotation -> Task.Task Effects.Never (Result String ())
saveDirectionAnnotation data =
  Http.send
    Http.defaultSettings
    { verb = "PUT"
    , headers = []
    , url = ("/directions.v1/" ++ (toString directionId))
    , body = Http.string (E.encode 0 ((E.object [("annotations", E.string (E.encode 0 (DirectionAnnotation.encode data)))])))
    }
    |> Http.fromJson D.value
    |> Task.toResult
    |> Task.map (formatError errorToString)
    |> Task.map (Result.map (\_ -> ()))


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
