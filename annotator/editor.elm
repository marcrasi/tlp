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
import Constants exposing (directionId, ourHeight, ourWidth)
import FrameNavigator.Component as FrameNavigator
import HttpUtil exposing (errorToString)
import Model.DirectionAnnotation as DirectionAnnotation exposing (DirectionAnnotation)
import Model.Frame exposing (Frame)
import RegionsEditor


type Action
  = RegionsEditorAction RegionsEditor.Action
  | FrameNavigatorAction FrameNavigator.Action
  | ReceiveAnnotation DirectionAnnotation
  | ReceiveError String
  | AutoSaveAction (AutoSave.Action DirectionAnnotation)


type LoadedRegions
  = LoadingRegions
  | LoadedRegions RegionsEditor.Model


type alias Model =
  { frameNavigator : FrameNavigator.Model 
  , loadedRegions : LoadedRegions
  , autoSave: AutoSave.Model DirectionAnnotation
  , error : Maybe String
  }


init : (Model, Effects.Effects Action)
init =
  let
    (autoSaveModel, autoSaveEffects) = AutoSave.init saveDirectionAnnotation
    (frameNavigatorModel, frameNavigatorEffects) = FrameNavigator.init directionId
  in
    ( { frameNavigator = frameNavigatorModel
      , loadedRegions = LoadingRegions 
      , autoSave = autoSaveModel
      , error = Nothing
      }
    , Effects.batch
      [ getDirection
      , Effects.map AutoSaveAction autoSaveEffects
      , Effects.map FrameNavigatorAction frameNavigatorEffects
      ]
    )


editorWithImage : Signal.Address Action -> Frame -> RegionsEditor.Model -> Html
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
        [ FrameNavigator.view (Signal.forwardTo address FrameNavigatorAction) model.frameNavigator
        ]
    ]


frameView : Signal.Address Action -> Frame -> RegionsEditor.Model -> Model -> Html
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
  let
    maybeError = Maybe.oneOf
      [ model.error
      , FrameNavigator.maybeError model.frameNavigator
      ]
  in
    case (FrameNavigator.maybeFrame model.frameNavigator, model.loadedRegions, maybeError) of
      (_, _, Just error) ->
        rootView (text ("Error: " ++ error))
      (Just frame, LoadedRegions regionsEditorModel, _) ->
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
            (autoSaveModel, autoSaveEffects) = AutoSave.update (AutoSave.DataChange newRegionsEditorModel.annotation) model.autoSave
          in
            ( { model
                | loadedRegions = LoadedRegions newRegionsEditorModel
                , autoSave = autoSaveModel
                }
            , Effects.map AutoSaveAction autoSaveEffects
            )
    FrameNavigatorAction frameNavigatorAction ->
      let
        (frameNavigatorModel, frameNavigatorEffects) = FrameNavigator.update frameNavigatorAction model.frameNavigator
      in
        ({ model | frameNavigator = frameNavigatorModel }, Effects.map FrameNavigatorAction frameNavigatorEffects)
    ReceiveAnnotation annotation ->
      ({ model | loadedRegions = LoadedRegions (RegionsEditor.init annotation) }, Effects.none)
    ReceiveError error -> ({ model | error = Just error }, Effects.none)
    AutoSaveAction autoSaveAction ->
      let
        (autoSaveModel, autoSaveEffects) = AutoSave.update autoSaveAction model.autoSave
      in
        ({ model | autoSave = autoSaveModel }, Effects.map AutoSaveAction autoSaveEffects)


getDirection : Effects.Effects Action
getDirection =
  Http.get DirectionAnnotation.decodeDirectionResponse ("/directions.v1/" ++ (toString directionId))
    |> Task.toResult
    |> Task.map interpretDirectionResponse
    |> Effects.task


interpretDirectionResponse : Result Http.Error DirectionAnnotation -> Action
interpretDirectionResponse result =
  case result of
    Ok directionAnnotation ->
      ReceiveAnnotation directionAnnotation
    Err error ->
      ReceiveError (errorToString error)


saveDirectionAnnotation : DirectionAnnotation -> Task.Task Effects.Never (Result String ())
saveDirectionAnnotation data =
  Http.send
    Http.defaultSettings
    { verb = "PUT"
    , headers = []
    , url = ("/directions.v1/" ++ (toString directionId))
    , body = Http.string (E.encode 0 (DirectionAnnotation.encodeDirectionUpdate data)) 
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
