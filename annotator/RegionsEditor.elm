module RegionsEditor (..) where


import List
import Maybe
import Signal
import Svg exposing (..)
import Svg.Events
import Svg.Attributes exposing (..)


import ListUtil
import Model.Frame
import Path exposing (Path, svgPathString)
import RegionEditor


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


canNewRegion : Model -> Bool
canNewRegion model =
  case model.state of
    NotEditing -> True
    _ -> False


canStopEdit : Model -> Bool 
canStopEdit model =
  case model.state of
    Editing _ _ -> True
    _ -> False


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


view : Signal.Address Action -> Model -> Svg
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
  in
    g
      []
      ([ regions address model ] ++ maybeEditor)


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
