module Main exposing (..)

import View.View exposing (view)
import Model.Model exposing (..)
import Update.Update exposing (update)
import Subscriptions.Subscriptions exposing (subscriptions)
import Json.Encode exposing (Value)
import Browser

-- Four parts: model, view, update:w
main: Program Json.Encode.Value Model Msg
main  = Browser.element { init = init, update = update, view = view, subscriptions = subscriptions}

