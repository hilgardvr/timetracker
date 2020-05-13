module View.LoginView exposing (loginView)

import Model.Model exposing (..)
import Element exposing (Element, width, centerY, row, fill, spacing, text, rgb255)
import Element.Input exposing (button, username, newPassword, labelAbove)
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
        [ viewNavBar
        , text "You are logged in"
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

viewNavBar: Element Msg
viewNavBar =
    row [ width fill, centerY, spacing 30 ]
        [ text "Time-me"
        ]

viewLoggedOut: Model -> Element Msg
viewLoggedOut model =
    row []
        [ viewNavBar
        , text "You are not logged in - your all changes will be lost when you leave the site"
        , text "Create a free account or log in"
        , username
            []
            { onChange = ChangeUserName 
            , text = model.userName
            , placeholder = Nothing
            , label = labelAbove [] (text "email")
            }
        , newPassword
            []
            { onChange = ChangePassword
            , text = model.password
            , placeholder = Nothing
            , label = labelAbove [] (text "password")
            , show = False
            }
        -- , input [ type_ "text", placeholder "email", value model.userName, onInput ChangeUserName ] []
        -- , input [ type_ "password", placeholder "password", value model.password, onInput ChangePassword ] []
        , button
            [ Background.color (rgb255 240 0 245)
            , Element.focused [ Background.color (rgb255 111 111 111) ]
            ]
            { onPress = Just CreateAccount
            , label = text "Create Account"
            } 
        , button
            [ Background.color (rgb255 240 0 245)
            , Element.focused [ Background.color (rgb255 111 111 111) ]
            ]
            { onPress = Just Login
            , label = text "Login"
            } 
        ]
