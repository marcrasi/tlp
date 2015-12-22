import Debug
import Html exposing (Html)
import Json.Decode exposing (Decoder, (:=), int, object2)
import Signal
import Svg exposing (..)
import Svg.Attributes exposing (..)
import VirtualDom

import MouseEvents exposing (..)

mb : Signal.Mailbox (Int, Int)
mb = Signal.mailbox (0, 0)


listener : Attribute
listener =
  onMouseOut (Signal.message mb.address)


view : (Int, Int) -> Html
view value =
  svg
    [ width "120", height "120", viewBox "0 0 120 120" ]
    [ rect [ x "10", y "10", width "100", height "100", rx "15", ry "15", listener ] [] ]


debugView : (Int, Int) -> Html
debugView value =
  view (Debug.log "click value" value) 


main : Signal Html
main =
  Signal.map debugView mb.signal
