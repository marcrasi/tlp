import Dict exposing (Dict)
import Effects
import Html exposing (..)
import Html.Events exposing (..)
import Http
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
import RegionsLabler


type Action
  = FrameNavigatorAction FrameNavigator.Action
  | RegionsLablerAction Int RegionsLabler.Action 
  | AutoSaveAction Int (AutoSave.Action Int)
  | ReceiveAnnotation DirectionAnnotation
  | ReceiveError String


type LoadedAnnotation
  = LoadingAnnotation
  | LoadedAnnotation DirectionAnnotation 


type alias Model =
  { frameNavigator : FrameNavigator.Model
  , loadedAnnotation : LoadedAnnotation
  , regionsLablers : Dict Int (RegionsLabler.Model, AutoSave.Model Int)
  , error : Maybe String
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
      (Just frame, LoadedAnnotation _, _) ->
        case Dict.get frame.id model.regionsLablers of
          Just (regionsLablerModel, _) ->
            rootView (frameView address frame regionsLablerModel model)
          Nothing ->
            text "This really really shouln't happen!"
      _ ->
        rootView (text "Loading...")


maybeInitializeRegionLabler : Model -> (Model, Effects.Effects Action)
maybeInitializeRegionLabler model =
  case (FrameNavigator.maybeFrame model.frameNavigator, model.loadedAnnotation) of
    (Just frame, LoadedAnnotation annotation) ->
      case Dict.get frame.id model.regionsLablers of
        Just _ ->
          (model, Effects.none)
        Nothing ->
          let
            (autoSaveModel, autoSaveEffects) = AutoSave.init (\x -> Task.succeed (Result.Ok ()))
            regionsLablerModel = RegionsLabler.init annotation
          in
            ( { model | regionsLablers = Dict.insert frame.id (regionsLablerModel, autoSaveModel) model.regionsLablers }
            , Effects.map (AutoSaveAction frame.id) autoSaveEffects
            )
    _ ->
      (model, Effects.none)


update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    FrameNavigatorAction frameNavigatorAction ->
      let
        (frameNavigatorModel, frameNavigatorEffects) = FrameNavigator.update frameNavigatorAction model.frameNavigator
        newModel1 = { model | frameNavigator = frameNavigatorModel }
        (newModel2, newModel2Effects) = maybeInitializeRegionLabler newModel1 
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
      ({ model | regionsLablers = Dict.update frameId (Maybe.map (\(regionsLablerModel, x) -> (RegionsLabler.update regionsLablerAction regionsLablerModel, x))) model.regionsLablers }, Effects.none)
    _ ->
      (model, Effects.none)


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
