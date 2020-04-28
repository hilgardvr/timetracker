module View.LoginView exposing (loginView)

import Html exposing (..)
import Model.Model exposing (..)
import Html.Events exposing (onInput, onClick)
-- import Html.Attributes exposing (type_, placeholder, value, selected, checked)
import Html.Attributes exposing (..)


loginView: Model -> Html Msg
loginView model =
    case model.loginStatus of
        LoggedIn -> viewLoggedIn
        Pending -> viewPending
        LoggedOut -> viewLoggedOut model

viewLoggedIn: Html Msg
viewLoggedIn = 
    div []
        [ text "You are logged in" ]

viewPending: Html Msg
viewPending =
    div []
        [ text "We are processing your login request"
        ]

viewLoggedOut: Model -> Html Msg
viewLoggedOut model =
    div []
        [ text "You are not logged in - your all changes will be lost when you leave the site"
        , br [] [] 
        , text "Create a free account or log in"
        , br [] [] 
        , input [ type_ "text", placeholder "email", value model.userName, onInput ChangeUserName ] []
        , input [ type_ "password", placeholder "password", value model.password, onInput ChangePassword ] []
        , button  
            [ onClick CreateAccount ]
            [ text "Create Account" ]
        , button  
            [ onClick Login ]
            [ text "Login" ]
        ]
