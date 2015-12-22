import Html
import Signal

import RegionEditor exposing (..)


mailbox : Signal.Mailbox Action
mailbox = Signal.mailbox (MouseMoveCanvas (0, 0))


initialModel : Model
initialModel =
  { state = Free
  , points = []
  , closed = False
  }


main : Signal Html.Html
main = 
  Signal.map (view mailbox.address) (Signal.foldp update initialModel mailbox.signal)
