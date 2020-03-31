module Update.Update exposing (..)

import Model.Model exposing(..)
import Time

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
            , Cmd.none
            )
        ChangeEditProject editProject ->
            ( { model | editingProject = editProject }
            , Cmd.none
            )
        DeleteCompleted deleteItem ->
            ( deleteCompleted model deleteItem
            , Cmd.none
            )

deleteCompleted: Model -> Completed -> Model
deleteCompleted model deleteItem =
    let
        filteredList = List.filter (\completedItem -> completedItem.id /= deleteItem.id) model.completedList
    in
        { model | completedList = filteredList }

editCompleted: Model -> Completed -> Model
editCompleted model completed =
    if model.editing 
    then 
        let
            editedCompleted = Completed completed.id model.editingProject completed.startTime completed.endTime completed.note
            editedList = 
                List.map 
                    (\comp -> 
                        if comp.id == completed.id 
                        then editedCompleted 
                        else comp
                    )
                model.completedList 
        in
            { model | completedList = editedList, editing = False, editingProject = model.currentProject }

    else { model | editing = True, editingId = completed.id, editingProject = completed.project }

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
                    , currentProject = ""
                    , note = ""
            }
    else { model | startTime = model.currentTime, timing = True }
