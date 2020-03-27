module Init.Init exposing (init)

import Model.Model exposing (..)
import Task
import Time

--init

init: () -> ( Model, Cmd Msg )
init _ = 
    ( Model [] False "" (Time.millisToPosix 0) (Time.millisToPosix 0) Time.utc []
    , Task.perform AdjustTimeZone Time.here
    )
