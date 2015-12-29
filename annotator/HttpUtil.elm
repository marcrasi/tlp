module HttpUtil (errorToString) where


import Http exposing (..)


errorToString : Error -> String
errorToString error =
  case error of
    BadResponse _ errorString -> errorString
    NetworkError -> "NetworkError"
    Timeout -> "Timeout"
    UnexpectedPayload errorString -> errorString
