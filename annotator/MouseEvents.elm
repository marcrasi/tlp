module MouseEvents
  ( onClick, onMouseDown, onMouseMove, onMouseOut, onMouseOver, onMouseUp ) where

{-|

VirtualDom mouse events with various positioning information that is not included in the Svg and Html events
modules.

-}

import Json.Decode exposing (Decoder, (:=), int, object2)
import Signal
import VirtualDom


positionDecoder : Decoder (Int, Int)
positionDecoder =
  object2 (,)
    ("offsetX" := int)
    ("offsetY" := int)


on : String -> ((Int, Int) -> Signal.Message) -> VirtualDom.Property
on name message =
  VirtualDom.on name positionDecoder message


onClick : ((Int, Int) -> Signal.Message) -> VirtualDom.Property
onClick = on "click"


onMouseDown : ((Int, Int) -> Signal.Message) -> VirtualDom.Property
onMouseDown = on "mousedown"


onMouseMove : ((Int, Int) -> Signal.Message) -> VirtualDom.Property
onMouseMove = on "mousemove"


onMouseOut : ((Int, Int) -> Signal.Message) -> VirtualDom.Property
onMouseOut = on "mouseout"


onMouseOver : ((Int, Int) -> Signal.Message) -> VirtualDom.Property
onMouseOver = on "mouseover"


onMouseUp : ((Int, Int) -> Signal.Message) -> VirtualDom.Property
onMouseUp = on "mouseup"

