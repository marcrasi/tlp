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
import Model.DirectionAnnotation exposing (DirectionAnnotation, Region)
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
  , annotation : DirectionAnnotation 
  }


init : DirectionAnnotation -> Model
init annotation =
  { state = NotEditing 
  , annotation = annotation
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
    (model.annotation.regions
      |> List.map (.path)
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
            newRegions = ListUtil.update index (\x -> { x | path = newEditorModel.path }) model.annotation.regions
            oldAnnotation = model.annotation
          in
            { model | state = Editing index newEditorModel, annotation = { oldAnnotation | regions = newRegions } } 
        _ ->
          model
    NewRegion ->
      let
        oldAnnotation = model.annotation
      in
        { model
        | annotation = { oldAnnotation | regions = (Region Nothing Path.empty) :: model.annotation.regions }
        , state = Editing 0 (RegionEditor.init Path.empty)
        }
    StartEdit index ->
      case ListUtil.get index model.annotation.regions of
        Just targetRegion ->
          { model | state = Editing index (RegionEditor.init targetRegion.path) }
        Nothing ->
          model
    StopEdit ->
      { model | state = NotEditing }
    DeleteRegion ->
      case model.state of
        Editing index _ ->
          let
            oldAnnotation = model.annotation
          in
            { model
              | annotation = { oldAnnotation | regions = ListUtil.remove index model.annotation.regions }
              , state = NotEditing
              }
        _ -> model 
