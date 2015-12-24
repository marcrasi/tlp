import Debug
import Html
import Signal

import RegionsEditor exposing (..)


mailbox : Signal.Mailbox Action
mailbox = Signal.mailbox NewRegion


main : Signal Html.Html
main = 
  Signal.map (view mailbox.address) (Signal.foldp (\a m -> Debug.log "model" (update a m)) init mailbox.signal)
