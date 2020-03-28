module Main exposing (..)

import View.View exposing (view)
import Model.Model exposing (..)
import Update.Update exposing (update)
import Subscriptions.Subscriptions exposing (subscriptions)
import Browser

-- Four parts: model, view, update:w
main: Program () Model Msg
main  = Browser.element { init = init, update = update, view = view, subscriptions = subscriptions}

