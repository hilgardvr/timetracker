module View.LoginView exposing (loginView, viewNavBar)

import Model.Model exposing (..)
import Element exposing (..)
import Element.Input as Input
import Element.Background as Background


primaryColor: Color
primaryColor = rgb255 157 255 209

lightColor: Color
lightColor = rgb255 209 255 255

darkColor: Color
darkColor = rgb255 106 203 160

loginView: Model -> Element Msg
loginView model =
    case model.loginStatus of
        LoggedIn -> viewLoggedIn
        Pending -> viewPending
        LoggedOut -> viewLoggedOut model
        Signup -> viewSignUpPage model

viewSignUpPage: Model -> Element Msg
viewSignUpPage model =
    column [ width fill, height fill ]
        [ viewNavBar model
        , row [ width fill, height (px 100) ] 
            [ el [ centerX, centerY ] <| text "Create a free account"
            ]
        , viewCreateAccountRow model
        ]

viewLoggedIn: Element Msg
viewLoggedIn = 
    row [ ]
        [ text "Time-me"
        , text "You are logged in"
        , Input.button
            [ Background.color primaryColor
            , Element.focused [ Background.color (rgb255 111 111 111) ]
            ]
            { onPress = Just Logout
            , label = text "Logout"
            } 
        ]

viewPending: Element Msg
viewPending =
    row []
        [ text "We are processing your login request"
        ]

viewNavBar: Model -> Element Msg
viewNavBar model = 
    let
        createOrLogoutButton = makeCreateOrLogOutButton model.loginStatus
    in
        row [ padding 20, Background.color primaryColor, width fill ]
            [ el [ Background.color darkColor] <| Element.text "Time-Me"
            , el [ centerX, Background.color lightColor ] <| text "Focus on the process"
            , createOrLogoutButton
            ]

makeCreateOrLogOutButton: LoginStatus -> Element Msg
makeCreateOrLogOutButton status =
    let
        (action, buttonText) = 
            case status of
                LoggedIn -> (Logout, "Logout")
                LoggedOut -> (CreateAccountPage, "Create Account")
                Model.Model.Signup -> (LoginPage, "Login")
                Pending -> (CreateAccountPage, "Create Account")
    in
        Input.button
            [ Background.color darkColor
            , Element.focused [ Background.color darkColor ]
            , alignRight
            ]
            { onPress = Just action
            , label = text buttonText
            } 


viewLoggedOut: Model -> Element Msg
viewLoggedOut model =
    column [ width fill, height fill ]
        [ viewNavBar model
        , row [ width fill, height (px 100) ] 
            [ el [ centerX, centerY ] <| text "Please login or create a free account"
            ]
            , viewLogginRow model
        ]

viewLogginRow: Model -> Element Msg
viewLogginRow model =
    row [ height (px 100), centerX ] [ 
        Input.username
            []
            { onChange = ChangeUserName 
            , text = model.userName
            , placeholder = Nothing
            , label = Input.labelAbove [] (text "email")
            }
        , Input.newPassword
            [ ]
            { onChange = ChangePassword
            , text = model.password
            , placeholder = Nothing
            , label = Input.labelAbove [] (text "password")
            , show = False
            }
        , Input.button
            [ Background.color darkColor
            , Element.focused [ Background.color (rgb255 111 111 111) ]
            ]
            { onPress = Just Login
            , label = text "Login"
            } 
        ]

viewCreateAccountRow: Model -> Element Msg
viewCreateAccountRow model =
    row [ centerX, height (px 100) ] [ 
        Input.username
            []
            { onChange = ChangeUserName 
            , text = model.userName
            , placeholder = Nothing
            , label = Input.labelAbove [] (text "email")
            }
        , Input.newPassword
            [ ]
            { onChange = ChangePassword
            , text = model.password
            , placeholder = Nothing
            , label = Input.labelAbove [] (text "password")
            , show = False
            }
        , Input.button
            [ Background.color darkColor
            , Element.focused [ Background.color (rgb255 111 111 111) ]
            ]
            { onPress = Just CreateAccount
            , label = text "Create Account"
            } 
        ]