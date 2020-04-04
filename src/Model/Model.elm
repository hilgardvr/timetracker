module Model.Model exposing (..)

import Time
import Task

--init

init: () -> ( Model, Cmd Msg )
init _ = 
    ( Model [] False "" (Time.millisToPosix 0) (Time.millisToPosix 0) Time.utc [] "" "" False "" "" "" (Time.millisToPosix 0) (Time.millisToPosix 0) Minute Minute timeFrameList
    , Task.perform AdjustTimeZone Time.here
    )

timeFrameList: List String
timeFrameList =
    [ "Year"
    , "Month"
    , "Day"
    , "Hour"
    , "Minute"
    , "Second"
    ]

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
    , editingProject: String
    , editingNote: String
    , editingStartTime: Time.Posix
    , editingEndTime: Time.Posix
    , editingStartTimeFrame: TimeFrame
    , editingEndTimeFrame: TimeFrame
    , timeFrameList: List String
    }

type alias Completed =
    { id: String
    , project: String
    , startTime: Time.Posix
    , endTime: Time.Posix
    , note: String
    }

type TimeFrame =
    Year
    | Month
    | Day
    | Hour
    | Minute
    | Second

type IncOrDec =
    Increment
    | Decrement

type StartOrEnd =
    Start
    | End

type Msg =
    ToggleTimer
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | NewProject String
    | AddProject
    | ChangeCurrentProject String
    | ChangeNote String
    | Editing Completed
    | ChangeEditProject String
    | ChangeEditNote String
    | ChangeEditTime StartOrEnd IncOrDec 
    | ChangeEditingStartTimeFrame String
    | ChangeEditingEndTimeFrame String
    | DeleteCompleted Completed
    | DiscardChanges
