module RegionsEditor (..) where


import Html exposing (..)
import Html.Events
import List
import Signal
import Svg exposing (..)
import Svg.Events
import Svg.Attributes exposing (..)


import ListUtil
import Path exposing (Path, svgPathString)
import RegionEditor


ourHeight : Int
ourHeight = 500


ourWidth : Int
ourWidth = 500


type State
  = Editing Int RegionEditor.Model
  | NotEditing


editingIndex : State -> Maybe Int
editingIndex state =
  case state of
    Editing index _ -> Just index
    NotEditing -> Nothing


type alias Model =
  { state : State
  , paths : List Path
  }


init : Model
init =
  { state = NotEditing 
  , paths = []
  }


type Action
  = EditorAction RegionEditor.Action
  | NewRegion
  | StartEdit Int
  | StopEdit


region : Signal.Address Action -> Int -> Path -> Svg
region address index path =
  Svg.path
    [ d (svgPathString path)
    , fill "none"
    , stroke "red"
    , strokeWidth "2"
    , pointerEvents "visible"
    , Svg.Events.onClick (Signal.message address (StartEdit index))
    ]
    []

regions : Signal.Address Action -> Model -> Svg
regions address model =
  g
    []
    (model.paths
      |> List.indexedMap (,)
      |> List.filter (\(i, x) -> Just i /= editingIndex model.state)
      |> List.map (\(i, x) -> region address i x))


view : Signal.Address Action -> Model -> Html
view address model =
  let
    maybeEditor = case model.state of
      Editing _ editorModel ->
        [ RegionEditor.view
          (Signal.forwardTo address EditorAction)
          editorModel
        ]
      _ ->
        []
    buttons = case model.state of
      Editing _ _ ->
        [ button [ Html.Events.onClick address StopEdit ] [ Html.text "Stop Editing" ] ]
      NotEditing ->
        [ button [ Html.Events.onClick address NewRegion ] [ Html.text "New Region" ] ]
  in
    div
      []
      ([ Svg.svg
          [ width (toString ourWidth) 
          , height (toString ourHeight) 
          ]
          (
            [ image
                [ xlinkHref "images/sample.jpg"
                , x "0"
                , y "0"
                , width (toString ourWidth)
                , height (toString ourHeight)
                ]
                []
            , regions address model 
            ] ++ maybeEditor)
      ] ++ buttons) 


update : Action -> Model -> Model
update action model =
  case action of
    EditorAction editorAction ->
      case model.state of
        Editing index editorModel ->
          let
            newEditorModel = RegionEditor.update editorAction editorModel
            newPaths = ListUtil.set index newEditorModel.path model.paths 
          in
            { model | state = Editing index newEditorModel, paths = newPaths }
        _ ->
          model
    NewRegion ->
      { model | paths = Path.empty :: model.paths, state = Editing 0 (RegionEditor.init Path.empty) }
    StartEdit index ->
      case ListUtil.get index model.paths of
        Just targetPath ->
          { model | state = Editing index (RegionEditor.init targetPath) }
        Nothing ->
          model
    StopEdit ->
      { model | state = NotEditing }
