module AutoSave (..) where


import Effects exposing (..)
import Html exposing (Html, text)
import Result exposing (..)
import Signal
import Task exposing (..)
import Time exposing (..)


debounce : Time
debounce = 1 * second


type Action d
  = DataChange d
  | SaveResult (Result String ())
  | Tick Time 


type State d
  = Saved
  | Saving
  | Changed d Time 
  | ChangedSaving d
  | Error String


type alias Model d =
  { state : State d
  , save : d -> Task Never (Result String ())
  , currentTime : Time
  }


init : (d -> Task Never (Result String ())) -> (Model d, Effects (Action d))
init save =
  ( { state = Saved
    , save = save
    , currentTime = 0
    }
  , tick Tick 
  )


update : Action d -> Model d -> (Model d, Effects (Action d))
update action model =
  case action of
    DataChange newData ->
      case model.state of
        Saving ->
          ({ model | state = ChangedSaving newData }, none)
        ChangedSaving _ ->
          ({ model | state = ChangedSaving newData }, none)
        _ ->
          ({ model | state = Changed newData model.currentTime }, none)
    SaveResult result ->
      case result of
        Ok _ ->
          case model.state of
            ChangedSaving data ->
              ({ model | state = Changed data model.currentTime }, none)
            _ ->
              ({ model | state = Saved }, none)
        Err error ->
          case model.state of
            ChangedSaving data ->
              ({ model | state = Changed data model.currentTime }, none)
            _ ->
              ({ model | state = Error error }, none)
    Tick currentTime ->
      case model.state of
        Changed data lastChangeTime ->
          if currentTime - lastChangeTime > debounce
            then
              ( { model | state = Saving, currentTime = currentTime }
              , batch
                  [ Effects.task <| Task.map SaveResult (model.save data)
                  , tick Tick
                  ]
              )
            else ({ model | currentTime = currentTime }, tick Tick)
        _ ->
          ({ model | currentTime = currentTime }, tick Tick)


view : Model d -> Html
view model =
  case model.state of
    Saved -> text "Saved"
    Saving -> text "Saving..."
    Changed _ _ -> text "Changed"
    ChangedSaving _ -> text "Saving..."
    Error error -> text ("Save Error: " ++ error)
