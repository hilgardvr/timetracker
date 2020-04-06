module Update.Update exposing (..)

import Time exposing (Month(..), toYear, toMonth, toDay)
import Model.Model exposing(..)
import Time
import Debug exposing (log)
import Task exposing (..)

-- update

update: Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        ToggleTimer -> 
            ( toggleTimer model
            , Cmd.none
            )
        Tick time -> 
            ( { model | currentTime = time }
            , Cmd.none
            )
        AdjustTimeZone zone -> 
            ( { model | timeZone = zone }
            , Cmd.none
            )
        NewProject newProject ->
            ( { model | newProject = newProject }
            , Cmd.none ) 
        AddProject ->
            ( addProject model
            , Cmd.none
            )
        ChangeCurrentProject currentProject ->
            ( { model | currentProject = currentProject }
            , Cmd.none
            )
        ChangeNote note ->
            ( { model | note = note }
            , Cmd.none
            )
        Editing completedItem -> 
            ( editCompleted model completedItem
            , Task.perform GetTimeNow Time.now
            )
        ChangeEditProject editProject ->
            ( { model | editingProject = editProject }
            , Cmd.none
            )
        ChangeEditNote editNote ->
            ( { model | editingNote = editNote }
            , Cmd.none
            )
        ChangeEditTime startOrEnd incOrDec ->
            ( changeEditTime model startOrEnd incOrDec
            , Cmd.none
            )
        ChangeEditingStartTimeFrame timeFrame ->
            ( { model | editingStartTimeFrame = getTimeFrameFromString timeFrame }
            , Cmd.none
            )
        ChangeEditingEndTimeFrame timeFrame ->
            ( { model | editingEndTimeFrame = getTimeFrameFromString timeFrame }
            , Cmd.none
            )
        DeleteCompleted deleteItem ->
            ( deleteCompleted model deleteItem
            , Cmd.none
            )
        DiscardChanges ->
            ( { model | editing = False, editingProject = model.currentProject, editingNote = "", editingStartTime = Time.millisToPosix 0, editingEndTime = Time.millisToPosix 0 }
            , Cmd.none
            )
        ShowCompletedFromDate date ->
            ( showCompletedChangeDates model FromDate date
            , Cmd.none
            )
        ShowCompletedToDate date ->
            ( showCompletedChangeDates model ToDate date
            , Cmd.none
            )
        ShowCompletedFromTime time ->
            ( showCompletedChangeDates model FromTime time
            , Cmd.none
            )
        ShowCompletedToTime time ->
            ( showCompletedChangeDates model ToTime time
            , Cmd.none
            )
        GetTimeNow time ->
            ( setCurrentTime model time
            , Cmd.none
            )



showCompletedChangeDates: Model -> FromOrTo -> String -> Model
showCompletedChangeDates model fromOrTo dateTime =
    case fromOrTo of
        FromDate -> 
            let 
                consoleLog = log "Date: " dateTime
                -- startOfDay

            in
                model
        ToDate ->
            let 
                consoleLog = log "Date: " dateTime
            in
                model
        FromTime ->
            let 
                consoleLog = log "Date: " dateTime
            in
                model
        ToTime ->
            let 
                consoleLog = log "Date: " dateTime
            in
                model




getTimeFrameFromString: String -> TimeFrame
getTimeFrameFromString timeFrame =
    case timeFrame of
        "Year" -> Year
        "Month" -> Month
        "Day" -> Day
        "Hour" -> Hour
        "Minute" -> Minute
        "Second" -> Second
        _ -> Second

changeEditTime: Model -> StartOrEnd -> IncOrDec -> Model
changeEditTime model startOrEnd incOrDec =
    let
        secs = 1000
        mins = secs * 60
        hours = mins * 60
        days = hours * 24
        months = days * 30
        years = days * 365
    in
        case startOrEnd of 
            Start ->
                case model.editingStartTimeFrame of
                    Second ->
                        case incOrDec of
                            Increment -> { model | editingStartTime = Time.millisToPosix (Time.posixToMillis model.editingStartTime + secs) }
                            Decrement -> { model | editingStartTime = Time.millisToPosix (Time.posixToMillis model.editingStartTime - secs) }
                    Minute ->
                        case incOrDec of
                            Increment -> { model | editingStartTime = Time.millisToPosix (Time.posixToMillis model.editingStartTime + mins) }
                            Decrement -> { model | editingStartTime = Time.millisToPosix (Time.posixToMillis model.editingStartTime - mins) }
                    Hour ->
                        case incOrDec of
                            Increment -> { model | editingStartTime = Time.millisToPosix (Time.posixToMillis model.editingStartTime + hours) }
                            Decrement -> { model | editingStartTime = Time.millisToPosix (Time.posixToMillis model.editingStartTime - hours) }
                    Day ->
                        case incOrDec of
                            Increment -> { model | editingStartTime = Time.millisToPosix (Time.posixToMillis model.editingStartTime + days) }
                            Decrement -> { model | editingStartTime = Time.millisToPosix (Time.posixToMillis model.editingStartTime - days) }
                    Month ->
                        case incOrDec of
                            Increment -> { model | editingStartTime = Time.millisToPosix (Time.posixToMillis model.editingStartTime + months) }
                            Decrement -> { model | editingStartTime = Time.millisToPosix (Time.posixToMillis model.editingStartTime - months) }
                    Year ->
                        case incOrDec of
                            Increment -> { model | editingStartTime = Time.millisToPosix (Time.posixToMillis model.editingStartTime + years) }
                            Decrement -> { model | editingStartTime = Time.millisToPosix (Time.posixToMillis model.editingStartTime - years) }
            End ->
                case model.editingEndTimeFrame of
                    Second ->
                        case incOrDec of
                            Increment -> { model | editingEndTime = Time.millisToPosix (Time.posixToMillis model.editingEndTime + secs) }
                            Decrement -> { model | editingEndTime = Time.millisToPosix (Time.posixToMillis model.editingEndTime - secs) }
                    Minute ->
                        case incOrDec of
                            Increment -> { model | editingEndTime = Time.millisToPosix (Time.posixToMillis model.editingEndTime + mins) }
                            Decrement -> { model | editingEndTime = Time.millisToPosix (Time.posixToMillis model.editingEndTime - mins) }
                    Hour ->
                        case incOrDec of
                            Increment -> { model | editingEndTime = Time.millisToPosix (Time.posixToMillis model.editingEndTime + hours) }
                            Decrement -> { model | editingEndTime = Time.millisToPosix (Time.posixToMillis model.editingEndTime - hours) }
                    Day ->
                        case incOrDec of
                            Increment -> { model | editingEndTime = Time.millisToPosix (Time.posixToMillis model.editingEndTime + days) }
                            Decrement -> { model | editingEndTime = Time.millisToPosix (Time.posixToMillis model.editingEndTime - days) }
                    Month ->
                        case incOrDec of
                            Increment -> { model | editingEndTime = Time.millisToPosix (Time.posixToMillis model.editingEndTime + months) }
                            Decrement -> { model | editingEndTime = Time.millisToPosix (Time.posixToMillis model.editingEndTime - months) }
                    Year ->
                        case incOrDec of
                            Increment -> { model | editingEndTime = Time.millisToPosix (Time.posixToMillis model.editingEndTime + years) }
                            Decrement -> { model | editingEndTime = Time.millisToPosix (Time.posixToMillis model.editingEndTime - years) }

deleteCompleted: Model -> Completed -> Model
deleteCompleted model deleteItem =
    let
        filteredList = List.filter (\completedItem -> completedItem.id /= deleteItem.id) model.completedList
    in
        { model | completedList = filteredList, editing = False }

editCompleted: Model -> Completed -> Model
editCompleted model completed =
    if model.editing 
    -- save updated info
    then 
        let
            validStartEnd = Time.posixToMillis model.editingStartTime < Time.posixToMillis model.editingEndTime
            startTime = if validStartEnd then model.editingStartTime else completed.startTime
            endTime = if validStartEnd then model.editingEndTime else completed.endTime
            note =
                if model.editingNote == ""
                then completed.note
                else model.editingNote
            editedCompleted = Completed completed.id model.editingProject startTime endTime note
            editedList = 
                List.map 
                    (\comp -> 
                        if comp.id == completed.id 
                        then editedCompleted 
                        else comp
                    )
                model.completedList 
        in
            { model | completedList = editedList, editing = False, editingProject = model.currentProject, editingNote = "", editingStartTime = Time.millisToPosix 0, editingEndTime = Time.millisToPosix 0 }

    -- show editing 
    else 
        { model | editing = True, editingId = completed.id, editingProject = completed.project, editingStartTime = completed.startTime, editingEndTime = completed.endTime }
    

padTime: String -> String
padTime time =
    if String.length time < 2
    then padTime ("0" ++ time)
    else time


setCurrentTime: Model -> Time.Posix -> Model
setCurrentTime model time =
    let
        nowDateString = todayString time
    in
        { model | getTimeNow = time, showCompletedFromDate = nowDateString, showCompletedToDate = nowDateString }

todayString: Time.Posix -> String
todayString now =
    let
        year = String.fromInt (toYear Time.utc now)
        month = padTime (String.fromInt (monthToInt (toMonth Time.utc now)))
        day = padTime (String.fromInt (toDay Time.utc now))
    in
       year ++ "-" ++ month ++ "-" ++ day 


addProject: Model -> Model
addProject model = 
    if List.length (List.filter ( \existingProjectName -> existingProjectName == model.newProject ) model.projectList) == 0
    then 
        { model   
        | projectList = model.newProject::model.projectList 
        , currentProject = model.newProject
        , newProject = ""
        }
    else model

toggleTimer: Model -> Model
toggleTimer model =
    if model.timing
    then 
        let completed = 
                { id = String.left 8 model.currentProject ++ String.fromInt (Time.posixToMillis model.startTime) ++ String.fromInt (Time.posixToMillis model.currentTime)
                , project = model.currentProject
                , startTime = model.startTime
                , endTime = model.currentTime
                , note = model.note
                }
        in
            { model | completedList = completed :: model.completedList
                    , timing = False
                    , note = ""
            }
    else { model | startTime = model.currentTime, timing = True }


monthToString: Time.Month -> String
monthToString month =
    case month of
        Jan -> "Jan"
        Feb -> "Feb"
        Mar -> "Mar"
        Apr -> "Apr"
        May -> "May"
        Jun -> "Jun"
        Jul -> "Jul"
        Aug -> "Aug"
        Sep -> "Sep"
        Oct -> "Oct"
        Nov -> "Nov"
        Dec -> "Dec"

monthToInt: Time.Month -> Int
monthToInt month =
    case month of
        Jan -> 1
        Feb -> 2
        Mar -> 3
        Apr -> 4
        May -> 5
        Jun -> 6
        Jul -> 7
        Aug -> 8
        Sep -> 9
        Oct -> 10
        Nov -> 11
        Dec -> 12