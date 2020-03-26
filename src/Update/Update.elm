module Update.Update exposing (..)

import Model.Model exposing(..)


-- update

update: Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        ToggleTimer -> 
            ( doTimer model
            , Cmd.none
            )
        ChangeActivity currentActivity -> 
            ( { model | currentActivity = currentActivity}
            , Cmd.none
            )
        Tick time -> 
            ( { model | currentTime = time }
            , Cmd.none
            )
        AdjustTimeZone zone -> 
            ( { model | timeZone = zone}
            , Cmd.none
            )

doTimer: Model -> Model
doTimer model =
    if model.timing
    then 
        let
            completed = 
                { activity = model.currentActivity
                , startTime = model.startTime
                , endTime = model.endTime
                }
        in
            { model | completed = completed :: model.completed,
                    endTime = model.currentTime, 
                    timing = False, 
                    currentActivity = ""
            }
    else { model | startTime = model.currentTime, timing = True }
