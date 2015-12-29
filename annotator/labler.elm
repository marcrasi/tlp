import Effects
import Html exposing (..)
import Html.Events exposing (..)
import Http
import StartApp
import Svg
import Svg.Attributes exposing (..)
import Task


import Constants exposing (directionId, ourHeight, ourWidth)
import FrameNavigator.Component as FrameNavigator
import HttpUtil exposing (errorToString)
import Model.DirectionAnnotation as DirectionAnnotation exposing (DirectionAnnotation)
import Model.Frame exposing (Frame)
import RegionsLabler


type Action
  = FrameNavigatorAction FrameNavigator.Action
  | ReceiveAnnotation DirectionAnnotation
  | ReceiveError String


type LoadedAnnotation
  = LoadingAnnotation
  | LoadedAnnotation RegionsLabler.Model 


type alias Model =
  { frameNavigator : FrameNavigator.Model
  , loadedAnnotation : LoadedAnnotation
  , error : Maybe String
  }


init : (Model, Effects.Effects Action)
init =
  let
    (frameNavigatorModel, frameNavigatorEffects) = FrameNavigator.init directionId
  in
    ( { frameNavigator = frameNavigatorModel
      , loadedAnnotation = LoadingAnnotation 
      , error = Nothing
      }
    , Effects.batch
      [ Effects.map FrameNavigatorAction frameNavigatorEffects
      , getDirection
      ]
    )


imageView : Frame -> RegionsLabler.Model -> Html
imageView frame regionsLablerModel =
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
    , RegionsLabler.view regionsLablerModel
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
        [ imageView frame regionsLablerModel
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
      (Just frame, LoadedAnnotation regionsLablerModel, _) ->
        rootView (frameView address frame regionsLablerModel model)
      _ ->
        rootView (text "Loading...")


update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    FrameNavigatorAction frameNavigatorAction ->
      let
        (frameNavigatorModel, frameNavigatorEffects) = FrameNavigator.update frameNavigatorAction model.frameNavigator
      in
        ({ model | frameNavigator = frameNavigatorModel }, Effects.map FrameNavigatorAction frameNavigatorEffects)
    ReceiveAnnotation annotation ->
      ({ model | loadedAnnotation = LoadedAnnotation (RegionsLabler.init annotation) }, Effects.none)
    ReceiveError error ->
      ({ model | error = Just error }, Effects.none)


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
