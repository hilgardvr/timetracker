module View.LoginView exposing (loginView, viewNavBar)

import View.Colors exposing (primaryColor, darkColor, lightColor, focussedColor, navBarHeight)
import Model.Model exposing (..)
import Element exposing (..)
import Element.Input as Input
import Element.Background as Background
import Element.Events as Events

loginView: Model -> Element Msg
loginView model =
    case model.loginStatus of
        LoggedIn -> viewLoggedIn model
        Pending -> viewPending
        LoggedOut -> viewLoggedOut model
        Signup -> viewSignUpPage model

viewSignUpPage: Model -> Element Msg
viewSignUpPage model =
    column [ width fill, height fill ]
        [ viewNavBar model
        , row [ width fill, height (px 100) ] 
            [ el [ centerX, centerY ] <| text "Please fill in details to create a free account"
            ]
        , viewCreateAccountRow model
        ]

viewLoggedIn: Model -> Element Msg
viewLoggedIn model = 
    viewNavBar model

viewPending: Element Msg
viewPending =
    row []
        [ text "We are processing your login request"
        ]

viewNavBar: Model -> Element Msg
viewNavBar model = 
    let
        createOrLogoutButton = makeCreateOrLogOutButton model.loginStatus

        historyButton = 
            if model.loginStatus == LoggedIn
            then
                Input.button
                    [ Background.color lightColor
                    , height <| px navBarHeight
                    , paddingXY 10 0
                    ]
                    { onPress = Just ShowHistory
                    , label = text "History"
                    }
            else none
    in
        row [ Background.color primaryColor, width fill, spacing 20 ]
            [ Input.button
                [ Background.color lightColor
                , height <| px navBarHeight
                , paddingXY 10 0
                ]
                { onPress = Just Home
                , label = text "timeme.org"
                }
            , el [ centerX ] <| text "Focus on the process"
            , historyButton
            , createOrLogoutButton
            ]

makeCreateOrLogOutButton: LoginStatus -> Element Msg
makeCreateOrLogOutButton status =
    let
        (action, buttonText) = 
            case status of
                LoggedIn -> (Logout, "Logout")
                LoggedOut -> (CreateAccountPage, "Create a free account?")
                Model.Model.Signup -> (LoginPage, "Already have an account?")
                Pending -> (CreateAccountPage, "Create Account")

    in
        Input.button
            [ Background.color lightColor
            , Element.focused [ Background.color focussedColor ]
            , alignRight
            , height <| px navBarHeight
            , paddingXY 10 0
            ]
            { onPress = Just action
            , label = text buttonText
            } 


viewLoggedOut: Model -> Element Msg
viewLoggedOut model =
    column [ width fill, height fill ]
        [ viewNavBar model
        , row [ width fill, height (px 100) ] 
            [ el [ centerX, centerY ] <| text "Please enter your account details to login"
            ]
            , viewLogginRow model
        ]

viewLogginRow: Model -> Element Msg
viewLogginRow model =
    column [ width fill, height fill, spacing <| 20 ]
        [ row [ centerX ] [ 
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
        ]
        , Input.button
            [ Background.color lightColor
            , Element.focused [ Background.color focussedColor ]
            , centerX
            ]
            { onPress = Just Login
            , label = el [ padding 10 ] <| text "Login"
            } 
        ]

viewCreateAccountRow: Model -> Element Msg
viewCreateAccountRow model =
    column [ width fill, height fill, spacing <| 20 ]
        [ row [ centerX ] [ 
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
        ] 
        , Input.button
            [ Background.color lightColor
            , Element.focused [ Background.color focussedColor ]
            , centerX
            ]
            { onPress = Just CreateAccount
            , label = el [ padding 10 ] <| text "Create Account"
            } 
        ]