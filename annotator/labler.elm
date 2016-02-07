import Dict exposing (Dict)
import Effects
import Html exposing (..)
import Html.Events exposing (..)
import Http
import Result exposing (formatError)
import StartApp
import Svg
import Svg.Attributes exposing (..)
import Task


import AutoSave
import Constants exposing (directionId, ourHeight, ourWidth)
import FrameNavigator.Component as FrameNavigator
import HttpUtil exposing (errorToString)
import Json.Decode as D
import Json.Encode as E
import Model.DirectionAnnotation as DirectionAnnotation exposing (DirectionAnnotation)
import Model.Frame exposing (Frame)
import Model.Labels as Labels exposing (Labels)
import RegionsLabler


type Action
  = FrameNavigatorAction FrameNavigator.Action
  | RegionsLablerAction Int RegionsLabler.Action
  | AutoSaveAction Int (AutoSave.Action Labels)
  | ReceiveAnnotation DirectionAnnotation
  | ReceiveError String


type LoadedAnnotation
  = LoadingAnnotation
  | LoadedAnnotation DirectionAnnotation


type alias Model =
  { frameNavigator : FrameNavigator.Model
  , loadedAnnotation : LoadedAnnotation
  , regionsLablers : Dict Int (RegionsLabler.Model, AutoSave.Model Labels)
  , error : Maybe String
  , autoLabelNewFrames : Bool
  }


init : (Model, Effects.Effects Action)
init =
  let
    (frameNavigatorModel, frameNavigatorEffects) = FrameNavigator.init directionId
  in
    ( { frameNavigator = frameNavigatorModel
      , loadedAnnotation = LoadingAnnotation
      , regionsLablers = Dict.empty
      , error = Nothing
      , autoLabelNewFrames = False
      }
    , Effects.batch
      [ Effects.map FrameNavigatorAction frameNavigatorEffects
      , getDirection
      ]
    )


imageView : Signal.Address RegionsLabler.Action -> Frame -> RegionsLabler.Model -> Html
imageView address frame regionsLablerModel =
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
    , RegionsLabler.view address regionsLablerModel
    ]


sideBar : Signal.Address Action -> Model -> Html
sideBar address model =
  div
    []
    [ div
        [ class "row"
        ]
        [ FrameNavigator.view (Signal.forwardTo address FrameNavigatorAction) model.frameNavigator
        ]
    ]


frameView : Signal.Address Action -> Frame -> RegionsLabler.Model -> Model -> Html
frameView address frame regionsLablerModel model =
  div
    [ class "row"
    ]
    [ div
        [ class "col-md-8"
        ]
        [ imageView (Signal.forwardTo address (RegionsLablerAction frame.id)) frame regionsLablerModel
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
  let
    maybeError = Maybe.oneOf
      [ model.error
      , FrameNavigator.maybeError model.frameNavigator
      ]
  in
    case (FrameNavigator.maybeFrame model.frameNavigator, model.loadedAnnotation, maybeError) of
      (_, _, Just error) ->
        rootView (text ("Error: " ++ error))
      (Just (frame, _), LoadedAnnotation _, _) ->
        case Dict.get frame.id model.regionsLablers of
          Just (regionsLablerModel, _) ->
            rootView (frameView address frame regionsLablerModel model)
          Nothing ->
            text "This really really shouln't happen!"
      _ ->
        rootView (text "Loading...")


maybeInitializeRegionLabler : Model -> Maybe Labels -> (Model, Effects.Effects Action)
maybeInitializeRegionLabler model defaultLabels =
  case (FrameNavigator.maybeFrame model.frameNavigator, model.loadedAnnotation) of
    (Just (frame, initialLabels), LoadedAnnotation annotation) ->
      case Dict.get frame.id model.regionsLablers of
        Just _ ->
          (model, Effects.none)
        Nothing ->
          let
            initialLabels2 =
              if List.length initialLabels.labels == 0 && model.autoLabelNewFrames
                then Maybe.withDefault (Labels []) defaultLabels
                else initialLabels
            (autoSaveModel, autoSaveEffects) = AutoSave.init (saveLabels frame.id)
            (autoSaveModel2, autoSaveEffects2) = AutoSave.update (AutoSave.DataChange initialLabels2) autoSaveModel
            regionsLablerModel = RegionsLabler.init annotation initialLabels2
          in
            ( { model | regionsLablers = Dict.insert frame.id (regionsLablerModel, autoSaveModel2) model.regionsLablers }
            , Effects.batch
              [ Effects.map (AutoSaveAction frame.id) autoSaveEffects
              , Effects.map (AutoSaveAction frame.id) autoSaveEffects2
              ]
            )
    _ ->
      (model, Effects.none)


update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    FrameNavigatorAction frameNavigatorAction ->
      let
        defaultLabels = case FrameNavigator.maybeFrame model.frameNavigator of
          Just (frame, _) ->
            case Dict.get frame.id model.regionsLablers of
              Just (regionsLablerModel, _) ->
                Just regionsLablerModel.labels
              Nothing ->
                Nothing
          Nothing ->
            Nothing
        (frameNavigatorModel, frameNavigatorEffects) = FrameNavigator.update frameNavigatorAction model.frameNavigator
        newModel1 = { model | frameNavigator = frameNavigatorModel }
        (newModel2, newModel2Effects) = maybeInitializeRegionLabler newModel1 defaultLabels
      in
        ( newModel2
        , Effects.batch
          [ Effects.map FrameNavigatorAction frameNavigatorEffects
          , newModel2Effects
          ]
        )
    ReceiveAnnotation annotation ->
      ({ model | loadedAnnotation = LoadedAnnotation annotation }, Effects.none)
    ReceiveError error ->
      ({ model | error = Just error }, Effects.none)
    RegionsLablerAction frameId regionsLablerAction ->
      let
        maybeUpdateResult =
          model.regionsLablers
            |> Dict.get frameId
            |> Maybe.map (\(x, y) -> (RegionsLabler.update regionsLablerAction x, y))
            |> Maybe.map (\(x, y) -> (x, AutoSave.update (AutoSave.DataChange x.labels) y))
        updatedRegionsLablers =
          maybeUpdateResult
            |> Maybe.map (\(x, (y, z)) -> Dict.insert frameId (x, y) model.regionsLablers)
            |> Maybe.withDefault model.regionsLablers
        autoSaveEffects =
          maybeUpdateResult
            |> Maybe.map (\(_, (_, x)) -> x)
            |> Maybe.withDefault Effects.none
      in
        ({ model | regionsLablers = updatedRegionsLablers }, Effects.map (AutoSaveAction frameId) autoSaveEffects)
    AutoSaveAction frameId autoSaveAction ->
      -- TODO: Should remove successfully saved things from dict, to avoid leaking memory.
      let
        maybeUpdateResult =
          model.regionsLablers
            |> Dict.get frameId
            |> Maybe.map (\(x, y) -> (x, AutoSave.update autoSaveAction y))
        updatedRegionsLablers =
          maybeUpdateResult
            |> Maybe.map (\(x, (y, z)) -> Dict.insert frameId (x, y) model.regionsLablers)
            |> Maybe.withDefault model.regionsLablers
        autoSaveEffects =
          maybeUpdateResult
            |> Maybe.map (\(_, (_, x)) -> x)
            |> Maybe.withDefault Effects.none
      in
        ({ model | regionsLablers = updatedRegionsLablers }, Effects.map (AutoSaveAction frameId) autoSaveEffects)


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


saveLabels : Int -> Labels -> Task.Task Effects.Never (Result String ())
saveLabels frameId labels =
  Http.send
    Http.defaultSettings
    { verb = "PUT"
    , headers = []
    , url = ("/frames.v1/" ++ (toString frameId))
    , body = Http.string (E.encode 0 (Labels.encodeLabels labels))
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
