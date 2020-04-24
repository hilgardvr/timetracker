module Model.Model exposing (..)

import Time exposing (Month(..))
import Task
import Http exposing (..)

--init

init: () -> ( Model, Cmd Msg )
init _ = 
    ( Model 
        [] 
        False 
        "" 
        (Time.millisToPosix 0) 
        (Time.millisToPosix 0) 
        Time.utc 
        [] 
        "" 
        "" 
        False 
        "" 
        "" 
        "" 
        (Time.millisToPosix 0) 
        (Time.millisToPosix 0) 
        Minute 
        Minute 
        timeFrameList 
        Minute
        Minute
        (Time.millisToPosix 0) 
        (Time.millisToPosix 0) 
        True
        False
        "All projects"
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
    , editingProject: String
    , editingNote: String
    , editingStartTime: Time.Posix
    , editingEndTime: Time.Posix
    , editingStartTimeFrame: TimeFrame
    , editingEndTimeFrame: TimeFrame
    , timeFrameList: List TimeFrame
    , completedFromTimeFrame: TimeFrame
    , completedToTimeFrame: TimeFrame
    , completedFromTime: Time.Posix
    , completedToTime: Time.Posix
    , showByStartTime: Bool
    , showByProject: Bool
    , projectShown: String
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

type FromOrTo =
    FromDate
    | FromTime
    | ToDate
    | ToTime

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
    | ChangeCompletedTime StartOrEnd IncOrDec 
    | ChangeCompletedFromTimeFrame String
    | ChangeCompletedToTimeFrame String
    | DeleteCompleted Completed
    | DiscardChanges
    | SetCompletedTimes Time.Posix
    | GotHistory (Result Http.Error (List Completed))
    | ToggleShowStarted
    | ToggleShowByProject
    | ChangeShowByProject String


timeFrameList: List TimeFrame
timeFrameList =
    [ Year
    , Month
    , Day
    , Hour
    , Minute
    , Second
    ]

timeFrameToString: TimeFrame -> String
timeFrameToString timeFrame =
    case timeFrame of
        Year -> "Year"
        Month -> "Month"
        Day -> "Day"
        Hour -> "Hour"
        Minute -> "Minute"
        Second -> "Second"