module Update.Update exposing (..)

import Model.Model exposing(..)


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

addProject: Model -> Model
addProject model = 
    if List.length (List.filter ( \m -> m == model.newProject ) model.projectList) == 0
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
                { project = model.currentProject
                , startTime = model.startTime
                , endTime = model.currentTime
                , note = model.note
                }
        in
            { model | completedList = completed :: model.completedList
                    , timing = False
                    , currentProject = ""
            }
    else { model | startTime = model.currentTime, timing = True }
