module Model.Model exposing (..)

import Time exposing (Month(..))
import Task
import Http exposing (..)
import Browser.Dom exposing (Viewport)
import Element exposing (Device, DeviceClass(..), Orientation(..))
import Json.Encode exposing (..)
import Json.Decode exposing (..)

--init

decoder: Json.Decode.Decoder SavedUserInfo
decoder =
    Json.Decode.map SavedUserInfo
        (Json.Decode.field "sttUserId" Json.Decode.string)

init: Json.Encode.Value -> ( Model, Cmd Msg )
init flags = 
    let
        savedInfo =
            case Json.Decode.decodeValue decoder flags of
                Ok savedId -> 
                    -- let
                    --    x = Debug.log "savedid:" savedId 
                    -- in
                        (LoggedIn, Just savedId.userId)
                Err err -> (Signup, Nothing)
    in
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
        ""
        "" 
        "" 
        (Time.millisToPosix 0) 
        (Time.millisToPosix 0) 
        Hour 
        Hour 
        timeFrameList 
        Hour 
        Hour 
        (Time.millisToPosix 0) 
        (Time.millisToPosix 0) 
        False
        False
        ""
        ""
        ""
        (Tuple.first savedInfo)
        (Tuple.second savedInfo)
        HomeScreen
        { class = Phone, orientation = Portrait }
        { width = 320, height = 550}
        False
        "An error occurred"
        ""
    , Task.succeed (LoginSavedUser (Tuple.second savedInfo)) |> Task.perform identity 
    )

identity: a -> a
identity a =
    a

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
    , userId: Maybe String
    , loggedInPage: LoggedInPage
    , device: Device
    , window:
        { width: Int
        , height: Int }
    , showDialog: Bool
    , dialogHeader: String
    , dialogBody: String
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
    | UserHashResult (Result Http.Error String)
    | CreatedItem (Result Http.Error Completed)
    | CreatedItemList (Result Http.Error (List Completed))
    | ItemDeleted (Result Http.Error String)
    | ItemUpdated (Result Http.Error Completed)
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
    | ShowHistory
    | Home
    | InitViewport Viewport
    | LoginSavedUser (Maybe String)
    | CloseDialog

type alias SavedUserInfo = 
    { userId: String }

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

type LoggedInPage =
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
