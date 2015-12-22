import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Signal

import RegionEditor exposing (..)

height : Int
height = 500

width : Int
width = 500

actions : Signal Action
actions =
  let
    pressChange = Signal.dropRepeats Mouse.isDown
    mouseDown = Signal.filterMap (\isDown -> if isDown then Just () else Nothing) () pressChange
    mouseUp = Signal.filterMap (\isDown -> if not isDown then Just () else Nothing) () pressChange
  in
    Signal.mergeMany
      [ Signal.map (\(x, y) -> MouseMove x y) Mouse.position
      , Signal.map (\(x, y) -> MouseUp x y) (Signal.sampleOn mouseUp Mouse.position)
      , Signal.map (\(x, y) -> MouseDown x y) (Signal.sampleOn mouseDown Mouse.position)
      ]

initialModel : Model
initialModel =
  { state = Free
  , points = []
  , closed = False
  }

main : Signal Element
main =
  Signal.map (\model -> collage width height [view model]) (Signal.foldp update initialModel actions) 
