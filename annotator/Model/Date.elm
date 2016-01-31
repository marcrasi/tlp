module Model.Date (dateString, unixSeconds) where


import Date exposing (Date, fromTime, fromString)
import Json.Decode exposing (..)


stringToDate : String -> Decoder Date
stringToDate str =
  case fromString str of
    Ok d -> succeed d
    Err err -> fail err


dateString : Decoder Date
dateString = string `andThen` stringToDate


unixSeconds : Decoder Date
unixSeconds = map ((\t -> 1000 * t) >> fromTime) float
