module Update.Update exposing (..)

import Time exposing (Month(..), toYear, toMonth, toDay, toHour, toMinute, toSecond, Posix)
import Model.Model exposing(..)
import Time
import Debug exposing (log)
import Task exposing (..)
import Http exposing (..)
import Json.Decode exposing (Decoder, int, string, field, map5, andThen, succeed)

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
            , Task.perform SetCompletedTimes Time.now
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
            , Cmd.none
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
        ChangeCompletedFromTimeFrame timeFrame ->
            ( { model | completedFromTimeFrame = getTimeFrameFromString timeFrame }
            , Cmd.none
            )
        ChangeCompletedToTimeFrame timeFrame ->
            ( { model | completedToTimeFrame = getTimeFrameFromString timeFrame }
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
        SetCompletedTimes time ->
            ( setCurrentTime model time
            , getUserHistory
            )
        ChangeCompletedTime startOrEnd incOrDec ->
            ( changeCompletedTime model startOrEnd incOrDec
            , Cmd.none
            )
        GotHistory result ->
            ( useFetchedHistory model result
            , Cmd.none
            ) 

getUserHistory: Cmd Msg
getUserHistory =
    Http.get
        { url = "http://localhost:9000/api/userhistory/1"
        , expect = Http.expectJson GotHistory completedListDecoder 
        }


completedListDecoder: Decoder (List Completed)
completedListDecoder =
    Json.Decode.list completedDecoder
    

completedDecoder: Decoder Completed
completedDecoder =
    Json.Decode.map5 Completed
        (field "id" string)
        (field "project" string)
        (field "startTime" timeDecoder)
        (field "endTime" timeDecoder)
        (field "note" string)

timeDecoder: Decoder Time.Posix
timeDecoder =
    int
        |> Json.Decode.andThen (\val -> Json.Decode.succeed <| Time.millisToPosix (1000 * val))

useFetchedHistory: Model -> (Result Http.Error (List Completed)) -> Model
useFetchedHistory model result =
    case result of
        Ok historyList -> 
            let
                projects = List.map (\item -> item.project) historyList
            in 
                { model 
                | completedList = historyList ++ model.completedList
                , projectList = projects ++ model.projectList
                }
        Err err -> 
            let
                y = Debug.log "error " err
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
        _ -> Minute

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

changeCompletedTime: Model -> StartOrEnd -> IncOrDec -> Model
changeCompletedTime model startOrEnd incOrDec =
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
                case model.completedFromTimeFrame of
                    Second ->
                        case incOrDec of
                            Increment -> { model | completedFromTime = Time.millisToPosix (Time.posixToMillis model.completedFromTime + secs) }
                            Decrement -> { model | completedFromTime = Time.millisToPosix (Time.posixToMillis model.completedFromTime - secs) }
                    Minute ->
                        case incOrDec of
                            Increment -> { model | completedFromTime = Time.millisToPosix (Time.posixToMillis model.completedFromTime + mins) }
                            Decrement -> { model | completedFromTime = Time.millisToPosix (Time.posixToMillis model.completedFromTime - mins) }
                    Hour ->
                        case incOrDec of
                            Increment -> { model | completedFromTime = Time.millisToPosix (Time.posixToMillis model.completedFromTime + hours) }
                            Decrement -> { model | completedFromTime = Time.millisToPosix (Time.posixToMillis model.completedFromTime - hours) }
                    Day ->
                        case incOrDec of
                            Increment -> { model | completedFromTime = Time.millisToPosix (Time.posixToMillis model.completedFromTime + days) }
                            Decrement -> { model | completedFromTime = Time.millisToPosix (Time.posixToMillis model.completedFromTime - days) }
                    Month ->
                        case incOrDec of
                            Increment -> { model | completedFromTime = Time.millisToPosix (Time.posixToMillis model.completedFromTime + months) }
                            Decrement -> { model | completedFromTime = Time.millisToPosix (Time.posixToMillis model.completedFromTime - months) }
                    Year ->
                        case incOrDec of
                            Increment -> { model | completedFromTime = Time.millisToPosix (Time.posixToMillis model.completedFromTime + years) }
                            Decrement -> { model | completedFromTime = Time.millisToPosix (Time.posixToMillis model.completedFromTime - years) }
            End ->
                case model.completedToTimeFrame of
                    Second ->
                        case incOrDec of
                            Increment -> { model | completedToTime = Time.millisToPosix (Time.posixToMillis model.completedToTime + secs) }
                            Decrement -> { model | completedToTime = Time.millisToPosix (Time.posixToMillis model.completedToTime - secs) }
                    Minute ->
                        case incOrDec of
                            Increment -> { model | completedToTime = Time.millisToPosix (Time.posixToMillis model.completedToTime + mins) }
                            Decrement -> { model | completedToTime = Time.millisToPosix (Time.posixToMillis model.completedToTime - mins) }
                    Hour ->
                        case incOrDec of
                            Increment -> { model | completedToTime = Time.millisToPosix (Time.posixToMillis model.completedToTime + hours) }
                            Decrement -> { model | completedToTime = Time.millisToPosix (Time.posixToMillis model.completedToTime - hours) }
                    Day ->
                        case incOrDec of
                            Increment -> { model | completedToTime = Time.millisToPosix (Time.posixToMillis model.completedToTime + days) }
                            Decrement -> { model | completedToTime = Time.millisToPosix (Time.posixToMillis model.completedToTime - days) }
                    Month ->
                        case incOrDec of
                            Increment -> { model | completedToTime = Time.millisToPosix (Time.posixToMillis model.completedToTime + months) }
                            Decrement -> { model | completedToTime = Time.millisToPosix (Time.posixToMillis model.completedToTime - months) }
                    Year ->
                        case incOrDec of
                            Increment -> { model | completedToTime = Time.millisToPosix (Time.posixToMillis model.completedToTime + years) }
                            Decrement -> { model | completedToTime = Time.millisToPosix (Time.posixToMillis model.completedToTime - years) }

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
        timeInt = Time.posixToMillis time
        startOfToday = timeInt - modBy 86400000 timeInt
        endOfToday = startOfToday + 86399000
        --if 
        hours = toHour model.timeZone (Time.millisToPosix startOfToday)

        millisToAdjust =
            if hours > 12
            then (24 - hours) * 3600000
            else -hours * 3600000

        startDayThisZone = Time.millisToPosix (startOfToday + millisToAdjust)
        endDayThisZone = Time.millisToPosix (endOfToday + millisToAdjust)
    in
        { model | completedFromTime = startDayThisZone, completedToTime = endDayThisZone }

timeToString: Time.Zone -> Time.Posix -> String
timeToString timeZone time =
    let
        year = String.fromInt (toYear timeZone time)
        month = padTime (String.fromInt (monthToInt (toMonth timeZone time)))
        day = padTime (String.fromInt (toDay timeZone time))
        hour = padTime (String.fromInt (toHour timeZone time))
        minute = padTime (String.fromInt (toMinute timeZone time))
        second = padTime (String.fromInt (toSecond timeZone time))
    in
       year ++ "-" ++ month ++ "-" ++ day  ++ " " ++ hour ++ ":" ++ minute ++ ":" ++ second


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