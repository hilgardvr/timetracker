module View.LoginView exposing (loginView)

import Html exposing (Html, input)
import Model.Model exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (type_, placeholder, value)
import Element exposing (Element, width, centerY, el, row, fill, alignRight, spacing, text, rgb255)
import Element.Input exposing (button)
import Element.Background as Background


loginView: Model -> Element Msg
loginView model =
    case model.loginStatus of
        LoggedIn -> viewLoggedIn
        Pending -> viewPending
        LoggedOut -> viewLoggedOut model

viewLoggedIn: Element Msg
viewLoggedIn = 
    row []
        [ text "You are logged in"
        , button
            [ Background.color (rgb255 240 0 245)
            , Element.focused [ Background.color (rgb255 111 111 111) ]
            ]
            { onPress = Just Logout
            , label = text "Logout"
            } 
            -- [ onClick Logout ]
            -- [ text "Logout"]
        ]

viewPending: Element Msg
viewPending =
    row []
        [ text "We are processing your login request"
        ]

viewNavBar =
    row [ width fill, centerY, spacing 30 ]
        [ text "Time-me"
        ]

viewLoggedOut: Model -> Element Msg
viewLoggedOut model =
    row []
        [ text "You are not logged in - your all changes will be lost when you leave the site"
        -- , br [] [] 
        , text "Create a free account or log in"
        -- , br [] [] 
        , input [ type_ "text", placeholder "email", value model.userName, onInput ChangeUserName ] []
        , input [ type_ "password", placeholder "password", value model.password, onInput ChangePassword ] []
        , button  
            [ onClick CreateAccount ]
            [ text "Create Account" ]
        , button  
            [ onClick Login ]
            [ text "Login" ]
        ]
