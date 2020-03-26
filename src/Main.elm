module Main exposing (..)

import View.View exposing (view)
import Model.Model exposing (..)
import Update.Update exposing (update)
import Browser
import Task 
import Time

-- Four parts: model, view, update:w
main: Program () Model Msg
main  = Browser.element { init = init, update = update, view = view, subscriptions = subscriptions}


--init

init: () -> ( Model, Cmd Msg )
init _ = 
    ( Model [] False "" (Time.millisToPosix 0) (Time.millisToPosix 0) (Time.millisToPosix 0) Time.utc
    , Task.perform AdjustTimeZone Time.here
    )

getCurrentTime: Time.Posix -> Time.Posix
getCurrentTime time = time

-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick
