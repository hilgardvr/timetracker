module Subscriptions.Subscriptions exposing (subscriptions)

import Time
import Model.Model exposing (..)
import Browser.Events
import Platform.Sub

-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subList =
            [ Time.every 1000 Tick
            , Browser.Events.onResize 
                (\w h -> 
                    let
                        fw = toFloat w
                        fh = toFloat h
                    in
                        InitViewport { scene = { width = fw, height = fh}, viewport = {x = fw, y = fh, width = fw, height = fh} }
                )
            ]
    in
        Platform.Sub.batch subList
    