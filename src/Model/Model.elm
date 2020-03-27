module Model.Model exposing (..)

import Time

-- model

type alias Completed =
    { activity: String
    , startTime: Time.Posix
    , endTime: Time.Posix
    }

type alias Model = 
    { completed: List Completed
    , timing: Bool
    , currentActivity: String
    , currentTime: Time.Posix
    , startTime: Time.Posix
    , timeZone: Time.Zone
    , porjects: List String
    }

type Msg =
    ToggleTimer
    | ChangeActivity String
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone