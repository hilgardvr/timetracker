
module View.FilterView exposing (filterHistory)

import Model.Model exposing (Model, Completed)
import Time exposing (..)


filterHistoryByStartTime: Model -> Completed -> Bool
filterHistoryByStartTime model completedItem =
    if model.showByStartTime
    then
        Time.posixToMillis completedItem.startTime > Time.posixToMillis model.completedFromTime &&
        Time.posixToMillis completedItem.startTime < Time.posixToMillis model.completedToTime
    else True

filterHistoryByProject: Model -> Completed -> Bool
filterHistoryByProject model completedItem = 
    if model.showFilterByProject
    then completedItem.project == model.projectShown 
    else True

filterHistory: Model -> List Completed
filterHistory model = 
    List.filter
        (\completedItem -> 
            filterHistoryByStartTime model completedItem && filterHistoryByProject model completedItem
        )
        model.completedList