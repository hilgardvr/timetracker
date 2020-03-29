module Model.Model exposing (..)

import Time
import Task

--init

init: () -> ( Model, Cmd Msg )
init _ = 
    ( Model [] False "" (Time.millisToPosix 0) (Time.millisToPosix 0) Time.utc [] "" "" False ""
    , Task.perform AdjustTimeZone Time.here
    )

-- model
type alias Model = 
    { completedList: List Completed
    , timing: Bool
    , currentProject: String
    , currentTime: Time.Posix
    , startTime: Time.Posix
    , timeZone: Time.Zone
    , projectList: List String
    , newProject: String
    , note: String
    , editing: Bool
    , editingId: String
    }

type alias Completed =
    { id: String
    , project: String
    , startTime: Time.Posix
    , endTime: Time.Posix
    , note: String
    }

type Msg =
    ToggleTimer
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | NewProject String
    | AddProject
    | ChangeCurrentProject String
    | EditNote String
    | Editing Completed
