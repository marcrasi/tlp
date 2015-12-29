module RegionsEditor (..) where


import Html
import Html.Attributes
import Html.Events
import List
import Maybe
import Signal
import Svg exposing (..)
import Svg.Events
import Svg.Attributes exposing (..)


import ListUtil
import Model.Frame
import Model.Path as Path exposing (Path, svgPathString)
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


canDeleteRegion : Model -> Bool
canDeleteRegion model =
  case model.state of
    Editing _ _ -> True
    _ -> False


type Action
  = EditorAction RegionEditor.Action
  | NewRegion
  | StartEdit Int
  | StopEdit
  | DeleteRegion


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


buttons : Signal.Address Action -> Model -> Html.Html
buttons address model =
  let
    newRegion = if canNewRegion model
      then
        [
          Html.button
            [ Html.Attributes.class "btn btn-success"
            , Html.Events.onClick address NewRegion
            ]
            [ Html.text "New Region"
            ]
        ]
      else []
    stopEdit = if canStopEdit model
      then 
        [
          Html.button
            [ Html.Attributes.class "btn btn-default"
            , Html.Events.onClick address StopEdit 
            ]
            [ Html.text "Stop Edit"
            ]
        ]
      else []
    deleteRegion = if canDeleteRegion model
      then
        [ Html.button
            [ Html.Attributes.class "btn btn-danger"
            , Html.Events.onClick address DeleteRegion
            ]
            [ Html.text "Delete Region"
            ]
        ]
      else []
  in
    Html.div
      [ Html.Attributes.class "btn-group"
      ]
      (newRegion ++ stopEdit ++ deleteRegion)


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
    DeleteRegion ->
      case model.state of
        Editing index _ ->
          { model | paths = ListUtil.remove index model.paths, state = NotEditing }
        _ -> model 
