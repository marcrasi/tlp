module Model.Path (Path, addPoint, close, empty, setPointPosition, svgPathString, encode, decoder) where


import Json.Decode exposing (..)
import Json.Encode as E
import List
import String


import ListUtil


type alias Path =
  { points : List (Int, Int)
  , closed : Bool
  }


decoder : Decoder Path
decoder =
  object2 Path
    ("points" := list (tuple2 (,) int int))
    ("closed" := bool)


encode : Path -> Value
encode path =
  E.object
    [ ("points", E.list (List.map (\(x, y) -> E.list [E.int x, E.int y]) path.points))
    , ("closed", E.bool path.closed)
    ]


empty : Path
empty =
  { points = []
  , closed = False
  }


close : Path -> Path
close path =
  { path | closed = True }


addPoint : (Int, Int) -> Path -> Path
addPoint point path =
  { path | points = point :: path.points }


setPointPosition : Int -> (Int, Int) -> Path -> Path
setPointPosition index p path =
  { path | points = ListUtil.set index p path.points } 


svgPathString : Path -> String
svgPathString path =
  let
    notClosedPathString = List.foldl addPointToPathString "" path.points
  in
    if path.closed
      then closePathString notClosedPathString
      else notClosedPathString


addPointToPathString : (Int, Int) -> String -> String
addPointToPathString (x, y) pathString =
  let
    command = if String.length pathString == 0 then "M" else "L"
  in
    pathString ++ " " ++ command ++ " " ++ toString x ++ " " ++ toString y


closePathString : String -> String
closePathString pathString = pathString ++ " Z"



