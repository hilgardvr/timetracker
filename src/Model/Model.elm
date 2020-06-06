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
        False 
        False 
        False 
        False 
        False 
        False 
        False 
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
        Day 
        Day 
        timeFrameList 
        Day 
        Day 
        (Time.millisToPosix 0) 
        (Time.millisToPosix 0) 
        False
        False
        ""
        ""
        ""
        LoggedOut
        Nothing
        HomeScreen
    , Task.perform AdjustTimeZone Time.here
    )


-- model

type alias Model = 
    { completedList: List Completed
    , timing: Bool
    , currentProject: String
    , showProjectDropDown: Bool
    , showTimeFrameFromDropDown: Bool
    , showTimeFrameToDropDown: Bool
    , showFilterByProjectDropDown: Bool
    , showEditingCompletedProjectDropDown: Bool
    , showEditingStartTimeDropDown: Bool
    , showEditingEndTimeDropDown: Bool
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
    , showFilterByProject: Bool
    , projectShown: String
    , userName: String
    , password: String
    , loginStatus: LoginStatus
    , userId: Maybe Int
    , loggedInPage: LoggedInPages
    }

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
    | ChangeShowByProject String
    | ChangeUserName String
    | ChangePassword String
    | Login
    | Logout
    | CreateAccount
    | GotHistory (Result Http.Error (List Completed))
    | UserIdResult (Result Http.Error Int)
    | CreatedItemId (Result Http.Error ())
    | CreatedItemList (Result Http.Error ())
    | ItemDeleted (Result Http.Error ())
    | ItemUpdated (Result Http.Error ())
    | GetUserHistory
    | CreateItemList
    | CreateAccountPage
    | LoginPage
    | ToggleProjectDropDown
    | CloseMenu
    | HandleFilterTimeChange TimeFrame StartOrEnd String
    | ToggleShowStarted Bool
    | ToggleTimeFrameFromDropDown
    | ToggleTimeFrameToDropDown
    | ToggleFilterProjectDropDown
    | ToggleShowFilterProject Bool
    | ToggleShowEditingCompletedProjectDropDown
    | ToggleShowEditingStartTimeDropDown
    | ToggleShowEditingEndTimeDropDown



type alias Completed =
    { id: String
    , project: String
    , startTime: Time.Posix
    , endTime: Time.Posix
    , note: String
    }

type LoginStatus =
    LoggedIn
    | LoggedOut
    | Signup
    | Pending

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

type LoggedInPages =
    HomeScreen 
    | Timing
    | History
    | EditingCompleted

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

secs: Int
secs = 1000
        
mins: Int        
mins = secs * 60

hours: Int
hours = mins * 60

days: Int
days = hours * 24

months: Int
months = days * 30

years: Int
years = days * 365