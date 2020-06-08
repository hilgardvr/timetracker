module Subscriptions.Subscriptions exposing (subscriptions)

import Time
import Model.Model exposing (..)

-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick
    