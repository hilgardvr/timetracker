
module Main exposing (..)

import Html exposing (div, input, Html, button, text, span)
import Browser
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (type_, placeholder, value)
import Time
import Task 

-- Four parts: model, view, update:w
main  = Browser.element { init = init, update = update, view = view, subscriptions = subscriptions}


-- model

type alias Model = 
    { timing: Bool
    , activity: String
    , currentTime: Time.Posix
    , startTime: Time.Posix
    , endTime: Time.Posix
    , timeZone: Time.Zone
    }

type Msg =
    ToggleTimer
    | ChangeActivity String
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone

--init

init: () -> ( Model, Cmd Msg )
init _ = 
    ( Model False "" (Time.millisToPosix 0) (Time.millisToPosix 0) (Time.millisToPosix 0) Time.utc
    , Task.perform AdjustTimeZone Time.here
    )

-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick

-- update

update: Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        ToggleTimer -> 
            ( { model | timing = not model.timing }
            , Cmd.none
            )
        ChangeActivity activity -> 
            ( { model | activity = activity}
            , Cmd.none
            )
        Tick time -> 
            ( { model | currentTime = time }
            , Cmd.none
            )
        AdjustTimeZone zone -> 
            ( { model | timeZone = zone}
            , Cmd.none
            )

-- view

view: Model -> Html Msg
view model =
        div []
            [ displayTime model.currentTime model.timeZone
            , input [ type_ "text", placeholder "What do you want to time?", value model.activity, onInput ChangeActivity ] []
            , button  
                [ onClick ToggleTimer ]
                [ if model.timing then text "Stop" else text "Start" ]
            ] 

    
displayTime: Time.Posix -> Time.Zone -> Html Msg
displayTime time zone =
    let
        hour    = String.fromInt (Time.toHour zone time)
        minute  = String.fromInt (Time.toMinute zone time)
        second  = String.fromInt (Time.toSecond zone time)
    in
        span [] [ text (hour ++ ":" ++ minute ++ ":" ++ second)]