module View.FilterShowCompleted exposing (..)

import Time exposing (..)
import Html exposing (..)
import Model.Model exposing (..)

filterShowCompleted: Model -> List Completed
filterShowCompleted model =
    model.completedList